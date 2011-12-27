;;; google-calendar-message.el --- Google Calender API implementation

;; Copyright (C) 2011 Free Software Foundation, Inc

;; Author: Olivier Sirven <slaanesh@thebuble.org>
;; Version: 0.1
;; Keywords: comm

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a binding to automatically import attached ical events from a message article
;; C-c i => create and attach a new calendar event
;;

;;; Code:

(eval-and-compile
  (require 'google-calendar))

(require 'mail-utils)

(defun message-create-google-calendar-event()
  "Create and attach a google calendar event to the current message.

If any email addresses are present in To and Cc headers, they will be
added as event attendees"
  (interactive)

  (let ((cc-content
         (save-restriction (message-narrow-to-headers)
                           (message-fetch-field "cc")))
        (to-content
         (save-restriction (message-narrow-to-headers)
                           (message-fetch-field "to")))
        (calendar-name (completing-read "Enter calendar to use: " (google-calendar-calendars-completion-list t)))
        (event-name (read-string "Event name: "))
        (event-start-date-time (google-calendar-read-date-time "Event starts on: "))
        (event-end-date-time (google-calendar-read-date-time "Event stops on: "))
        (description (read-string "Description (if any): "))
        attendees)


    (if (and cc-content to-content)
          (setq to-content (concat to-content ", " cc-content)))

    (dolist (email (split-string (concat to-content ", " cc-content) " ?, ?" t))
      (setq attendees (vconcat (list (list (cons "email" email))) attendees)))

    (let (params)
      (save-excursion
        (if attendees
            (setq params (list (cons "attendees"  attendees))))

        (goto-char (point-max))
        (insert (concat "<#part type=\"text/calendar\" filename=\""
                        (google-calendar-export-to-ical
                         (vector
                          (google-calendar-add-event calendar-name
                                                     event-name
                                                     event-start-date-time
                                                     event-end-date-time
                                                     description
                                                     params)))
                        "\" disposition=attachment>\n<#/part>"))))))

(define-key message-mode-map (kbd "C-c i") 'message-create-google-calendar-event)

(provide 'google-calendar-message)
