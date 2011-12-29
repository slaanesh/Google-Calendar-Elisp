;;; google-calendar-gnus.el --- Google Calender API implementation

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

;; Adds a binding to automatically import attached ical events from a gnus article
;;

;;; Code:

(eval-and-compile
  (require 'google-calendar))

(require 'gnus-art)
(require 'mm-decode)

(defun gnus-article-import-ical-handle-to-google-calendar(handle)
  "Import current mime handle into google calendar"

  (let ((continue (equal (mm-handle-media-type handle) "text/calendar")))
    (if (not continue)
        (setq continue (yes-or-no-p "Part is not an icalendar mime part. Continue?")))

    (cond
     (continue

      (save-excursion
        (gnus-mime-copy-part handle)
        (let ((calendar-name (completing-read "Enter calendar to use: " (google-calendar-calendars-completion-list t))))
          (goto-char (point-min))
          (insert (mm-get-part handle))
          (google-calendar-import-ical-buffer calendar-name (current-buffer)))
        (message "Event imported")
        (kill-buffer))))))


(defun gnus-article-import-ical-to-google-calendar(n)
  "Import current article part as an ical event into google calendar"
  (interactive "P")
  (gnus-article-part-wrapper n 'gnus-article-import-ical-handle-to-google-calendar))

(eval-after-load "gnus"
  '(define-key gnus-article-mode-map (kbd "Ki") 'gnus-article-import-ical-to-google-calendar))

(provide 'google-calendar-gnus)
