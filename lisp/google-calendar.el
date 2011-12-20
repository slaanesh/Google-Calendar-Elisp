;;; google-calender.el --- Google Calender API implementation

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

;; Implementation of the Google Calendar API
;;

;;; Code:

(require 'url-cache)
(require 'oauth2)
(require 'json)
(require 'org)

(defconst google-calendar-api-uri "https://www.googleapis.com/calendar/v3"
  "Base URI of the Google Calendar REST API server")

(defconst google-calendar-client-id "784466018688.apps.googleusercontent.com"
  "Google Client ID for this application")

(defconst google-calendar-client-secret "qsPYZu1VS53I0L7UrZJWhXfK"
  "Google client (not so) secret for this application")

(defconst google-calendar-redirect-uri "https://www.googleapis.com/auth/calendar"
  "Google Oauth2 redirect URI")

(defconst google-calendar-oauth-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst google-calendar-oauth-token-url "https://accounts.google.com/o/oauth2/token"
  "Google OAuth2 server URL.")

(defvar google-calendar-oauth-token nil "Google API OAuth2 authentication token")

(defcustom google-calendar-expire-time 3600
  "Time to keep calendars entry in cache without refreshing."
  :group 'google-calendar
  :type 'integer)

(defun google-calendar-auth-and-store()
  "Request a OAuth2 token and store it"
  (unless google-calendar-oauth-token
    (setq google-calendar-oauth-token
          (oauth2-auth-and-store
           google-calendar-oauth-auth-url
           google-calendar-oauth-token-url
           google-calendar-redirect-uri
           google-calendar-client-id
           google-calendar-client-secret)))
  google-calendar-oauth-token)

(defun google-calendar-url-retrieve (url)
  "Retrieve URL using cache if possible."
  (let ((url-cache-expire-time google-calendar-expire-time))
    (if (url-cache-expired url)
        (let ((buf (oauth2-url-retrieve-synchronously (google-calendar-auth-and-store)
                                                      url)))
          ;; This is `url-store-in-cache' modified so it uses
          ;; `google-calendar-api-uri' to store the cache file as the
          ;; current URL, rathen than the URL with the access token.
          (with-current-buffer buf
            (let ((fname (url-cache-create-filename url)))
              (if (url-cache-prepare fname)
                  (let ((coding-system-for-write 'binary))
                    (write-region (point-min) (point-max) fname nil 5)))))
          buf)
      (url-fetch-from-cache url))))

(defun google-calendar-build-full-url (resource &optional namespace)
  "Create a full google calendar API url"
  (concat google-calendar-api-uri "/" (or namespace "") (if namespace "/" "") resource))

(defun google-calendar-http-data (buffer)
  "Return HTTP data from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((headers (buffer-substring (point-min) (point))))
      (unless (string-match-p "^HTTP/1.1 200 OK" headers)
        (let ((error-msg "Unable to fetch data"))
          (if (string-match-p "\"error\"" (buffer-substring (point) (point-max)))
                (setq error-msg (cdr (assoc 'message (assoc 'error (json-read))))))
          (kill-buffer)
          (error error-msg)))
      (if (string-match-p "^Content-Type:.* charset=UTF-8" headers)
          (set-buffer-multibyte t)
        (set-buffer-multibyte nil)))
    (json-read)))

(defun google-calendar-list-all-calendars()
  "List all available calendars found in the authenticated google account"
  (setq url-request-method nil)
  (setq url-request-data nil)
  (google-calendar-http-data (google-calendar-url-retrieve (google-calendar-build-full-url "users/me/calendarList"))))

(defun google-calendar-writable-calendars-vector()
  "Generate a vector of all available writable calendars or nil if none are found"

  (let ((data (cdr (assoc 'items (google-calendar-list-all-calendars))))
        (res (vconcat)))
    (loop for calendar across data do
      (dolist (calendar-property calendar)
        (if (and (equal 'accessRole (car calendar-property))
                 (string= "owner" (cdr calendar-property)))
            (setq res (vconcat res (list calendar)))
          )))
    res))

(defun google-calendar-calendars-completion-list(&optional writable)
  "Generate a list of calendars usable for completion

If WRITABLE is non nil then only writable calendars are returned"
  (let ((data (if writable
                  (google-calendar-writable-calendars-vector)
                (cdr (assoc 'items (google-calendar-list-all-calendars)))))
        (res '()))
    (loop for calendar across data do
          (let ((calendar-list (list))
                calendar-id
                calendar-name)
            (dolist (calendar-property calendar)
              (if (equal 'id (car calendar-property))
                  (setq calendar-id (cdr calendar-property))
                (if (equal 'summary (car calendar-property))
                    (setq calendar-name (cdr calendar-property))
                  )))
            (setq res (cons (list calendar-name calendar-id) res))
            ))
    res))

(defun google-calendar-get-id-by-name (name)
  "Returns a google calendar ID from its name"
  (let ((cal-id))
    (loop for calendar across (cdr (assoc 'items (google-calendar-list-all-calendars))) do
          (dolist (calendar-property calendar)
            (if (and (equal 'summary (car calendar-property))
                     (string= name (cdr calendar-property)))
                (setq cal-id (cdr (assoc 'id calendar))))))

    (if (not (string-match "@group\.calendar\.google\.com$" cal-id))
        "primary"
      cal-id)))

(defun google-calendar-read-value (prompt acceptable)
  "Read a value from the mini buffer and validate its value against ACCEPTABLE function"
  (let ((value (read-minibuffer prompt)))
    (while (not (funcall acceptable value))
      (setq value (read-minibuffer prompt)))
    value))

(defun google-calendar-read-date-time (&optional prompt)
  "Prompt the user for a date and a time and returns a RFC3999 date time string"
  (format-time-string
   "%Y-%m-%dT%H:%M:%S.00%z"
   (org-read-date t t nil prompt)))

(defun google-calendar-get-events(calendar-name)
  "Retrieve events from a google calendar"
  (google-calendar-http-data (google-calendar-url-retrieve (google-calendar-build-full-url (concat (google-calendar-get-id-by-name calendar-name) "/events") "calendars"))))

;;;###autoload
(defun google-calendar-add-event(calendar-name event-name event-start-date-time event-end-date-time &optional description)
  "Create a new event on a google calendar

If called non interactively CALENDAR-NAME represents the name of the calendar
you want to update, EVENT-NAME the name of the event to date,
EVENT-START-DATE-TIME a RFC3999 date time to identify the start time of
the event and EVENT-END-DATE-TIME the end RCF3999 date time.
DESCRIPTION would receive an optional description of the event"
  (interactive
   (list
    (completing-read "Enter calendar to use: " (google-calendar-calendars-completion-list t))
    (read-string "Event name: ")
    (google-calendar-read-date-time "Event starts on: ")
    (google-calendar-read-date-time "Event stops on: ")
    (read-string "Description (if any): ")))
  (let
      ((request-method "POST")
       (request-data
        (json-encode
         (list (cons "description" (or description ""))
               (cons "summary" event-name)
               (cons "start" (list (cons "dateTime" event-start-date-time)))
               (cons "end" (list (cons "dateTime" event-end-date-time))))))
       (request-extra-headers
             '(("Content-Type" . "application/json"))))
    (google-calendar-http-data
     (oauth2-url-retrieve-synchronously
      (google-calendar-auth-and-store)
      (google-calendar-build-full-url
       (concat (google-calendar-get-id-by-name calendar-name) "/events")
       "calendars")
      request-method request-data request-extra-headers))))

(provide 'google-calendar)

;;; google-calendar.el ends here
