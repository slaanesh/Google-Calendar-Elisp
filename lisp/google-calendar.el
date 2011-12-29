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
(require 'icalendar)

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

(defcustom google-calendar-account-email-address nil
  "Email address associated to the google account (if left nil, will be automatically guess)"
  :group 'google-calendar
  :type 'string)

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

(defun google-calendar-account-email-address()
  "Returns the email address of the authentication email address"
  (if (not google-calendar-account-email-address)
      (loop for calendar across (google-calendar-writable-calendars-vector) do
          (dolist (calendar-property calendar)
            (if (and (equal 'summary (car calendar-property))
                     (not (string-match "@group\.calendar\.google\.com$"
                                        (cdr (assoc 'id calendar)))))
                (setq google-calendar-account-email-address (cdr (assoc 'id calendar)))))))

  google-calendar-account-email-address)

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
  "Prompt the user for a date and a time and returns a RFC3339 date time string"
  (format-time-string
   "%Y-%m-%dT%H:%M:%S.00%z"
   (org-read-date t t nil prompt)))

(defun google-calendar-ical-format-string(event prop)
  "Correctly format a string field extracted from an iCal event"
  (icalendar--convert-string-for-import (icalendar--get-event-property event prop)))

(defun google-calendar-ical-format-email(str)
  "Extract an email address from an iCal property string"

  (if (string-match "^mailto:\\(.*\\)$" str)
      (setq str (match-string 1 str)))

  ;; icalendar--get-event-property badly interpret indentation of multi lines
  ;; so we are just making sure it won't break email addresses
  (replace-in-string str " " ""))

(defun google-calendar-ical-format-attendees(event)
  "Correctly format email attendees field from an iCal event.

Adds authenticated user email address if not present."
  (let ((attendees (icalendar--get-event-properties event 'ATTENDEE))
        (json [])
        email google-calendar-email-found)
    (if attendees
        (dolist (attendee attendees json)
          (setq email (google-calendar-ical-format-email attendee))
          (if (and
               (not google-calendar-email-found)
               (string= email (google-calendar-account-email-address)))
              (setq google-calendar-email-found t))

          (setq json (vconcat json (vector (list (cons "email" email)))))))

    (if (not google-calendar-email-found)
        (setq json (vconcat json (vector (list (cons "email" (google-calendar-account-email-address)))))))

    json))

(defun google-calendar-ical-format-cn(event prop)
  "Correctly format a CN field extracted from an iCal event"
  (let (json display-name
        (email (google-calendar-ical-format-email (icalendar--convert-string-for-import (icalendar--get-event-property event prop))))
        (cn (icalendar--get-event-property-attributes event prop)))

    (setq json (list (cons "email" email)))
    (if cn
        (setq display-name (nth 1 cn)))

    (if display-name
        (append json
                (list (cons "displayName" display-name)))
      json)))

(defun google-calendar-ical-get-events(buffer)
  "Parse a buffer and extracts all iCal events from there.
Returns a list whose first element is the events list and second one is the timezones map"

  (with-current-buffer (icalendar--get-unfolded-buffer buffer)
    (goto-char (point-min))
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (let (ical-list)
          ;; read ical
          (beginning-of-line)
          (setq ical-list (icalendar--read-element nil nil))
          (list (icalendar--all-events ical-list) (icalendar--convert-all-timezones ical-list)))
      (error "Current buffer does not contain iCalendar contents"))))

(defun google-calendar-decode-isodatetime(isodatetimestring &optional day-shift
                                                        zone)
  "Return ISODATETIMESTRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If
ISODATETIMESTRING specifies UTC time (trailing letter Z) the
decoded time is given in the local time zone!  If optional
parameter DAY-SHIFT is non-nil the result is shifted by DAY-SHIFT
days.
ZONE, if provided, is the timezone, in any format understood by `encode-time'.

FIXME: multiple comma-separated values should be allowed!"
  (icalendar--dmsg isodatetimestring)
  (if isodatetimestring
      ;; day/month/year must be present
      (let ((year  (read (substring isodatetimestring 0 4)))
            (month (read (substring isodatetimestring 5 7)))
            (day   (read (substring isodatetimestring 8 10)))
            (hour 0)
            (minute 0)
            (second 0))
        (when (> (length isodatetimestring) 11)
          ;; hour/minute present
          (setq hour (read (substring isodatetimestring 11 13)))
          (setq minute (read (substring isodatetimestring 14 16))))
        (when (> (length isodatetimestring) 16)
          ;; seconds present
          (setq second (read (substring isodatetimestring 17 19))))
        (when (and (> (length isodatetimestring) 19)
                   ;; UTC specifier present
                   (char-equal ?Z (aref isodatetimestring 19)))
          ;; if not UTC add current-time-zone offset
          (setq second (+ (car (current-time-zone)) second)))
        ;; shift if necessary
        (if day-shift
            (let ((mdy (calendar-gregorian-from-absolute
                        (+ (calendar-absolute-from-gregorian
                            (list month day year))
                           day-shift))))
              (setq month (nth 0 mdy))
              (setq day   (nth 1 mdy))
              (setq year  (nth 2 mdy))))
        ;; create the decoded date-time
        ;; FIXME!?!
        (condition-case nil
            (decode-time (encode-time second minute hour day month year zone))
          (error
           (message "Cannot decode \"%s\"" isodatetimestring)
           ;; hope for the best...
           (list second minute hour day month year 0 nil 0))))
    ;; isodatetimestring == nil
    nil))

(defun google-calendar-ical-event-to-json(event zone-map)
  "Parse the content of an iCal event into a json representation understable by google calendar"

  (let* (json-data diary-string
         (dtstart (icalendar--get-event-property event 'DTSTART))
         (dtstart-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes
                         event 'DTSTART)
                        zone-map))
         (dtstart-dec (google-calendar-decode-isodatetime dtstart nil
                                                     dtstart-zone))
         (start-d (icalendar--datetime-to-iso-date
                   dtstart-dec "-"))
         (start-t (icalendar--datetime-to-colontime dtstart-dec))
         (dtend (icalendar--get-event-property event 'DTEND))
         (dtend-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes
                       event 'DTEND)
                      zone-map))
         (dtend-dec (google-calendar-decode-isodatetime dtend
                                                   nil dtend-zone))
         (dtend-1-dec (google-calendar-decode-isodatetime dtend -1
                                                     dtend-zone))
         end-d
         end-1-d
         end-t
         (summary (icalendar--convert-string-for-import
                   (or (icalendar--get-event-property event 'SUMMARY)
                       "No summary")))
         (rrule (icalendar--get-event-property event 'RRULE))
         (rdate (icalendar--get-event-property event 'RDATE))
         (exdate (icalendar--get-event-property event 'EXDATE))
         (duration (icalendar--get-event-property event 'DURATION))
         event-ok)
    (message (format "%s: `%s'" start-d summary))
    (setq json-data (list (cons "summary" summary)))
    ;; check whether start-time is missing
    (if  (and dtstart
              (string=
               (cadr (icalendar--get-event-property-attributes
                      event 'DTSTART))
               "DATE"))
        (setq start-t nil))
    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
                          dtstart-dec
                          (icalendar--decode-isoduration duration)))
            (dtend-1-dec-d (icalendar--add-decoded-times
                            dtstart-dec
                            (icalendar--decode-isoduration duration
                                                           t))))
        (if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
            (message "Inconsistent endtime and duration for %s"
                     summary))
        (setq dtend-dec dtend-dec-d)
        (setq dtend-1-dec dtend-1-dec-d)))
    (setq end-d (if dtend-dec
                    (icalendar--datetime-to-iso-date dtend-dec "-")
                  start-d))
    (setq end-1-d (if dtend-1-dec
                      (icalendar--datetime-to-iso-date dtend-1-dec "-")
                    start-d))
    (setq end-t (if (and
                     dtend-dec
                     (not (string=
                           (cadr
                            (icalendar--get-event-property-attributes
                             event 'DTEND))
                           "DATE")))
                    (icalendar--datetime-to-colontime dtend-dec)
                  start-t))

    (setq json-data (append json-data (list
                                       (cons "start"
                                             (list
                                              (cons
                                               "dateTime"
                                               (format-time-string
                                                "%Y-%m-%dT%H:%M:%S.00%z"
                                                (date-to-time (concat start-d " " start-t (format-time-string "%z"))))))))))
    (setq json-data (append json-data (list
                                       (cons "end"
                                             (list
                                              (cons
                                               "dateTime"
                                               (format-time-string
                                                "%Y-%m-%dT%H:%M:%S.00%z"
                                                (date-to-time (concat end-d " " end-t (format-time-string "%z"))))))))))

    (let ((recurrence []))
      (if rrule
          (setq recurrence (vconcat recurrence (vector rrule))))
      (if rdate
          (setq recurrence (vconcat recurrence (vector rdate))))
      (if exdate
          (setq recurrence (vconcat recurrence (vector exdate))))

      (if (> (length recurrence) 0)
          (progn
            (setq event-ok t)
            (setq json-data (append json-data (list (cons "recurrence" recurrence)))))))

     ;; non-recurring event
     ;; all-day event
    (cond
     ((not (string= start-d end-d))
      (setq event-ok t))

     ;; not all-day
     ((and start-t (or (not end-t)
                       (not (string= start-t end-t))))
      (setq event-ok t))
     ;; all-day event
     (t
      (setq event-ok t)))

    ;; attendees
    (let ((attendees-data (google-calendar-ical-format-attendees event)))
      (if attendees-data
          (setq json-data (append json-data (list (cons "attendees" attendees-data))))))

    ;; other "simple" properties
    (let ((conversion-list
           '(("description" DESCRIPTION google-calendar-ical-format-string)
             ("location" LOCATION google-calendar-ical-format-string)
             ("visibility" PUBLIC google-calendar-ical-format-string)
             ("organizer" ORGANIZER google-calendar-ical-format-cn)
             ("htmlLink" URL google-calendar-ical-format-string)
             ("iCalUID" UID google-calendar-ical-format-string)
             ("status" STATUS google-calendar-ical-format-string))))

      (mapc (lambda (i)
              (let* ((json-name (car i))
                     (prop (cadr i))
                     (format (car (cddr i)))
                     (contents (icalendar--get-event-property event prop)))
                (when (and contents (> (length contents) 0))
                  (setq json-data
                        (append json-data
                                (list
                                 (cons json-name
                                       (funcall format event prop)))))
                  )))
            conversion-list))

    (if event-ok
        (json-encode json-data)
      (error (format "Cannot handle this event: %s" event)))))

(defun google-calendar-get-events(calendar-name)
  "Retrieve events from a google calendar"
  (cdr (assoc 'items (google-calendar-http-data (google-calendar-url-retrieve (google-calendar-build-full-url (concat (google-calendar-get-id-by-name calendar-name) "/events") "calendars"))))))

(defun google-calendar-query-events(calendar-name query &optional fields)
  "Search for QUERY in FIELDS in CALENDAR-NAME events and returns matching events.

If specified, FIELDS is a list of Google Calendar symbol. Default value is
`(list 'summary 'description)'."

  (if (not fields)
      (setq fields (list 'summary 'description)))

  (let ((events []) (case-fold-search t) match in-searched-property current-id)
    (loop for calendar across (google-calendar-get-events calendar-name) do
          (setq current-id nil
                match nil
                in-searched-property nil)
          (dolist (calendar-property calendar)
            (dolist (field fields)
              (if (equal field (car calendar-property))
                  (setq in-searched-property t)))
            (if (and in-searched-property
                     (string-match query (cdr calendar-property)))
                (setq match t)))
          (if match
              (setq events (vconcat events (vector calendar)))))
  events))

(defun google-calendar-ical-convert-datetime(dt)
  "Convert a google calender date time following RFC 3339 into an ical representration"
  (format-time-string
   "%Y-%m-%d %H:%M:%SZ"
   (date-to-time
    (if (string-match "^\\(.*\\)\\+\\([0-9]+\\):\\([0-9]+\\)$" dt)
        (concat (match-string 1 dt) "+" (match-string 2 dt) (match-string 3 dt))
      dt)) t))

(defun google-calendar-ical-fold-string(str)
  "Fold a string to follow ical format"
  (if (< (length str) 75)
      str
    (concat (substring str 0 74) "\n\t" (google-calendar-ical-fold-string (substring str 74)))))

(defun google-calendar-event-to-ical(event)
  "Convert a single google calendar event into an iCal string"

  (let ((ical-str "\nBEGIN:VEVENT")
        (attendees (assoc 'attendees event))
        (recurrence (assoc 'recurrence event))
        (organizer (assoc 'organizer event))
        (description (assoc 'description event))
        (location (assoc 'location event))
        (url (assoc 'htmlLink event))
        (creation (assoc 'created event))
        (updated (assoc 'updated event))
        (sequence (assoc 'sequence event))
        (class (assoc 'visibility event))
        )
    (setq ical-str
          (concat ical-str
                  "\nUID:" (google-calendar-ical-fold-string (cdr (or (assoc 'iCalUID event) (assoc 'id event))))
                  "\nSUMMARY:" (google-calendar-ical-fold-string (cdr (assoc 'summary event)))
                  "\nDTSTART:" (google-calendar-ical-convert-datetime (cdr (assoc 'dateTime (cdr (assoc 'start event)))))
                  "\nDTEND:" (google-calendar-ical-convert-datetime (cdr (assoc 'dateTime (cdr (assoc 'end event)))))
                  ))


     ;; description
     (if description
      (setq ical-str
            (concat ical-str
                    (google-calendar-ical-fold-string (concat "\nDESCRIPTION:" (cdr description))))))

     ;; location
     (if location
      (setq ical-str
            (concat ical-str
                    (google-calendar-ical-fold-string (concat "\nLOCATION:" (cdr location))))))

     ;; url
     (if url
      (setq ical-str
            (concat ical-str
                    (google-calendar-ical-fold-string (concat "\nURL:" (cdr url))))))

     ;; creation
     (if creation
      (setq ical-str
            (concat ical-str
                    (concat "\nCREATED:" (google-calendar-ical-convert-datetime (cdr creation))))))

     ;; updated
     (if updated
      (setq ical-str
            (concat ical-str
                    (concat "\nLAST-MODIFIED:" (google-calendar-ical-convert-datetime (cdr updated))))))

     ;; sequence
     (if sequence
      (setq ical-str
            (concat ical-str
                    (google-calendar-ical-fold-string (concat "\nSEQUENCE:" (int-to-string (cdr sequence)))))))

     ;; class
     (if class
      (setq ical-str
            (concat ical-str
                    (google-calendar-ical-fold-string (concat "\nCLASS:" (cdr class))))))

     ;; attendees
     (if attendees
      (loop for attendee across (cdr attendees) do
            (let ((email (cdr (assoc 'email attendee)))
                  (display-name (assoc 'displayName attendee)))
              (if display-name
                  (setq display-name (cdr display-name))
                (setq display-name email))

              (setq ical-str
                    (concat ical-str
                            (google-calendar-ical-fold-string (concat "\nATTENDEE;CN=" display-name ":mailto:" email)))))))

     ;; recurrence
     (if recurrence
      (loop for rule across (cdr recurrence) do
            (setq ical-str
                  (concat ical-str
                          (google-calendar-ical-fold-string rule)))))

     ;; organizer
     (if organizer
      (setq organizer (cdr organizer))
      (let ((email (cdr (assoc 'email organizer)))
                  (display-name (assoc 'displayName organizer)))
              (if display-name
                  (setq display-name (cdr display-name))
                (setq display-name email))

              (setq ical-str
                    (concat ical-str
                            (google-calendar-ical-fold-string (concat "\nORGANIZER;CN=" display-name ":mailto:" email))))))

    (concat ical-str "\nEND:VEVENT")))

(defun google-calendar-export-to-ical(events &optional ical-filename)
  "Export a list of events to iCal format

If ICAL-FILENAME is not provided, a temporary file will be used.

The function returns ICAL-FILENAME"

  (if (not ical-filename)
      (setq ical-filename (make-temp-file "vcal")))

  (with-current-buffer (find-file-noselect ical-filename)
    (let ((coding-system-for-write 'utf-8))
      (insert "BEGIN:VCALENDAR")
      (insert "\nPRODID:-//Emacs//NONSGML google-calendar.el//EN")
      (insert "\nVERSION:2.0")
      (loop for event across events do
            (insert (google-calendar-event-to-ical event)))
      (insert "\nEND:VCALENDAR\n")
      (save-buffer)
      (kill-buffer)))

  ical-filename)

;;;###autoload
(defun google-calendar-add-event(calendar-name event-name event-start-date-time event-end-date-time &optional description additional-fields)
  "Create a new event on a google calendar

If called non interactively CALENDAR-NAME represents the name of the calendar
you want to update, EVENT-NAME the name of the event to date,
EVENT-START-DATE-TIME a RFC3339 date time to identify the start time of
the event and EVENT-END-DATE-TIME the end RCF3339 date time.
DESCRIPTION would receive an optional description of the event.

When called non interactively ADDITIONAL-FIELDS can be used to specify the other
optional fields of the event. It's supposed to be a vector of
(list (cons 'property-name property-value)). For example:
(list (cons 'attendees (vector (list (cons \"email\" \"john.doe@example.com\")))))"
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
         (append
          (list (cons "description" (or description ""))
                (cons "summary" event-name)
                (cons "start" (list (cons "dateTime" event-start-date-time)))
                (cons "end" (list (cons "dateTime" event-end-date-time))))
          additional-fields)))
       (request-extra-headers
             '(("Content-Type" . "application/json"))))
    (google-calendar-http-data
     (oauth2-url-retrieve-synchronously
      (google-calendar-auth-and-store)
      (google-calendar-build-full-url
       (concat (google-calendar-get-id-by-name calendar-name) "/events")
       "calendars")
      request-method request-data request-extra-headers))))

;;;###autoload
(defun google-calendar-import-ical-region(calendar-name)
  "Import the current region as an iCal event into google calendar.

If called non interactively CALENDAR-NAME represents the name of the calendar
you want to update"
  (interactive
   (list
    (completing-read "Enter calendar to use: " (google-calendar-calendars-completion-list t))))

  (unless (use-region-p)
    (error "No active region"))

  (let
      ((ical-string (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (insert ical-string)
      (goto-char (point-min))
      (google-calendar-import-ical-buffer calendar-name (current-buffer)))))

;;;###autoload
(defun google-calendar-import-ical-buffer(calendar-name buffer)
  "Import a buffer containing an iCal data into calendar name CALENDAR-NAME"
  (interactive
   (list
    (completing-read "Enter calendar to use: " (google-calendar-calendars-completion-list t))
    (read-buffer "Buffer containing ical data to import: ")))

  (with-current-buffer buffer
    (let ((ical-data (google-calendar-ical-get-events buffer))
          (request-extra-headers
             '(("Content-Type" . "application/json")))
          events zone-map event request-data)

      (setq events (nth 0 ical-data))
      (setq zone-map (nth 1 ical-data))
      (while events
        (setq event (car events)
              events (cdr events)
              request-data (google-calendar-ical-event-to-json event zone-map))
        (google-calendar-http-data
         (oauth2-url-retrieve-synchronously
          (google-calendar-auth-and-store)
          (google-calendar-build-full-url
           (concat (google-calendar-get-id-by-name calendar-name) "/events/import")
           "calendars")
          "POST" request-data request-extra-headers))))))

(provide 'google-calendar)

;;; google-calendar.el ends here
