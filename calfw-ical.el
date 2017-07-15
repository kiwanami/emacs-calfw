;;; calfw-ical.el --- calendar view for ical format -*- lexical-binding: t -*-

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar
;; Package-Requires: ((cl-lib "0.5")(calfw "1.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A bridge from ical to calfw.
;; The API and interfaces have not been confirmed yet.

;;; Installation:

;; Here is a minimum sample code:
;; (require 'calfw-ical)
;; To open a calendar buffer, execute the following function.
;; (calfw-ical-open-calendar "http://www.google.com/calendar/ical/.../basic.ics")

;; Executing the following command, this program clears caches to refresh the ICS data.
;; (calfw-ical-data-cache-clear-all)

;;; Code:

(require 'cl-lib)
(require 'calfw)
(require 'icalendar)
(require 'url)
(require 'pp)

(defvar calfw-ical-zone-map nil)

(defun calfw-ical-event-get-dates (event)
  "Return date-time information from iCalendar event object:
period event (list 'period start-date end-date), time span
event (list 'time date start-time end-time).  The period includes
end-date.  This function is copied from
`icalendar--convert-ical-to-diary' and modified.  Recursive
events have not been supported yet."
  (let* ((dtstart (icalendar--get-event-property event 'DTSTART))
         (dtstart-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes event 'DTSTART)
                        calfw-ical-zone-map))
         (dtstart-dec (icalendar--decode-isodatetime dtstart nil dtstart-zone))
         (start-d (calfw-decode-to-calendar dtstart-dec))
         (start-t (calfw-time (nth 2 dtstart-dec) (nth 1 dtstart-dec)))

         (dtend (icalendar--get-event-property event 'DTEND))
         (dtend-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes event 'DTEND)
                      calfw-ical-zone-map))
         (dtend-dec (icalendar--decode-isodatetime dtend nil dtend-zone))
         (dtend-1-dec (icalendar--decode-isodatetime dtend -1 dtend-zone))

         (duration (icalendar--get-event-property event 'DURATION))

         end-d end-1-d end-t)

    (when (and dtstart
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
                            (icalendar--decode-isoduration duration t))))
        (if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
            (message "Inconsistent endtime and duration for %s" dtend-dec))
        (setq dtend-dec dtend-dec-d)
        (setq dtend-1-dec dtend-1-dec-d)))
    (setq end-d (if dtend-dec
                    (calfw-decode-to-calendar dtend-dec)
                  start-d))
    (setq end-1-d (if dtend-1-dec
                      (calfw-decode-to-calendar dtend-1-dec)
                    start-d))
    (setq end-t (if (and
                     dtend-dec
                     (not (string=
                           (cadr
                            (icalendar--get-event-property-attributes
                             event 'DTEND))
                           "DATE")))
                    (calfw-time (nth 2 dtend-dec) (nth 1 dtend-dec))
                  start-t))
    (cond
     ((and start-t (equal start-d end-d))
      (list 'time start-d start-t end-t))
     ((equal start-d end-1-d)
      (list 'time start-d nil nil))
     (t
      (list 'period start-d nil end-1-d)))))

(defun calfw-ical-sanitize-string (string)
  (when (and string
             (> (length string) 0))
    (replace-regexp-in-string "\\\\n" "\n"
                              (replace-regexp-in-string "\\\\," "," string))))

(defun calfw-ical-convert-event (event)
  (cl-destructuring-bind (dtag date start end) (calfw-ical-event-get-dates event)
    (make-calfw-event
     :start-date  date
     :start-time  start
     :end-date    (when (equal dtag 'period) end)
     :end-time    (when (equal dtag 'time)   end)
     :title       (calfw-ical-sanitize-string
                   (icalendar--get-event-property event 'SUMMARY))
     :location    (calfw-ical-sanitize-string
                   (icalendar--get-event-property event 'LOCATION))
     :description (calfw-ical-sanitize-string
                   (icalendar--get-event-property event 'DESCRIPTION)))))

(defun calfw-ical-convert-ical-to-calfw (ical-list)
  (cl-loop with calfw-ical-zone-map = (icalendar--convert-all-timezones ical-list)
           for e in (icalendar--all-events ical-list)
           for event = (calfw-ical-convert-event e)
           if event
           if (calfw-event-end-date event)
           collect event into periods
           else
           collect event into contents
           else do
           (progn
             (message "Ignoring event \"%s\"" e)
             (message "Cannot handle this event, tag: %s" e))
           finally (cl-return `((periods ,periods) ,@contents))))

(defun calfw-ical-debug (f)
  (interactive)
  (let ((buf (calfw-ical-url-to-buffer f)))
    (unwind-protect
        (pp-display-expression
         (with-current-buffer buf
           (calfw-ical-normalize-buffer)
           (calfw-ical-convert-ical-to-calfw
            (icalendar--read-element nil nil)))
         "*ical-debug*")
      (kill-buffer buf))))

(defvar calfw-ical-calendar-external-shell-command "wget -q -O - ")
(defvar calfw-ical-calendar-tmpbuf " *calfw-tmp*")
(defvar calfw-ical-url-to-buffer-get 'calfw-ical-url-to-buffer-internal)

(defun calfw-ical-url-to-buffer-external (url)
  "Retrieve ICS file with an external command."
  (let ((buf (get-buffer-create calfw-ical-calendar-tmpbuf)))
    (buffer-disable-undo buf)
    (with-current-buffer buf
      (erase-buffer))
    (call-process-shell-command
     (concat calfw-ical-calendar-external-shell-command " " url)
     nil buf nil)
    buf))

(defun calfw-ical-url-to-buffer-internal (url)
  "Retrieve ICS file with the url package."
  (let ((buf (url-retrieve-synchronously url))
        (dbuf (get-buffer-create calfw-ical-calendar-tmpbuf))
        pos)
    ;; FIXME: The origin is:
    ;;  (setq pos (url-http-symbol-value-in-buffer
    ;;             'url-http-end-of-headers buf)
    ;; but I can not find `url-http-symbol-value-in-buffer'
    ;; function, so I just set pos to 0, this may not work
    ;; properly.
    (unwind-protect
        (when (setq pos 0)
          (with-current-buffer dbuf
            (erase-buffer)
            (decode-coding-string
             (with-current-buffer buf
               (buffer-substring (1+ pos) (point-max)))
             'utf-8 nil dbuf)))
      (kill-buffer buf))
    dbuf))

(defun calfw-ical-url-to-buffer (url)
  (let* ((url-code (url-generic-parse-url url))
         (type (url-type url-code)))
    (cond
     (type
      (funcall calfw-ical-url-to-buffer-get url))
     (t ; assume local file
      (let ((buf (find-file-noselect (expand-file-name url) t)))
        (with-current-buffer buf (set-visited-file-name nil))
        buf)))))

(defmacro calfw-ical-with-buffer (url &rest body)
  (let (($buf (cl-gensym)))
    `(let ((,$buf (calfw-ical-url-to-buffer ,url)))
       (unwind-protect
           (with-current-buffer ,$buf
             (goto-char (point-min))
             ,@body)
         (kill-buffer ,$buf)))))
(put 'calfw-ical-with-buffer 'lisp-indent-function 1)

(defun calfw-ical-normalize-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n " nil t)
      (replace-match "")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "DT\\(START\\|END\\);VALUE=DATE:" nil t)
      (replace-match "DT\\1:")))
  (set-buffer-modified-p nil))

(defvar calfw-ical-data-cache nil "a list of (url . ics-data)")

(defun calfw-ical-data-cache-clear (url)
  (setq calfw-ical-data-cache
        (cl-loop for i in calfw-ical-data-cache
                 for (u . d) = i
                 unless (equal u url)
                 collect i)))

(defun calfw-ical-data-cache-clear-all ()
  (interactive)
  (setq calfw-ical-data-cache nil))

(defun calfw-ical-get-data (url)
  (let ((data (assoc url calfw-ical-data-cache)))
    (unless data
      (setq data (let ((cal-list
                        (calfw-ical-with-buffer url
                          (calfw-ical-normalize-buffer)
                          (calfw-ical-convert-ical-to-calfw
                           (icalendar--read-element nil nil)))))
                   (cons url cal-list)))
      (push data calfw-ical-data-cache))
    (cdr data)))

(defun calfw-ical-to-calendar (url begin end)
  (cl-loop for event in (calfw-ical-get-data url)
           if (and (listp event)
                   (equal 'periods (car event)))
           collect
           (cons
            'periods
            (cl-loop for evt in (cadr event)
                     if (and
                         (calfw-date-less-equal-p begin (calfw-event-end-date evt))
                         (calfw-date-less-equal-p (calfw-event-start-date evt) end))
                     collect evt))
           else if (calfw-date-between begin end (calfw-event-start-date event))
           collect event))

(defun calfw-ical-create-source (name url color)
  (let ((url url))
    (make-calfw-source
     :name (concat "iCal:" name)
     :color color
     :update (lambda () (calfw-ical-data-cache-clear url))
     :data (lambda (begin end)
             (calfw-ical-to-calendar url begin end)))))

(defun calfw-ical-open-calendar (url)
  "Simple calendar interface. This command displays just one
calendar source."
  (interactive)
  (save-excursion
    (let ((cp (calfw-create-calendar-component-buffer
               :view 'month
               :contents-sources
               (list (calfw-ical-create-source "ical" url "#2952a3")))))
      (switch-to-buffer (calfw-cp-get-buffer cp)))))

(provide 'calfw-ical)

;;; calfw-ical.el ends here
