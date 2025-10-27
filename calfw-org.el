;;; calfw-org.el --- Calendar view for org-agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Version: 2.0
;; Keywords: calendar, org
;; Package-Requires: ((emacs "28.1") (calfw "2.0"))
;; URL: https://github.com/haji-ali/emacs-calfw

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

;; Display org-agenda items in the calfw buffer.
;; (Because I don't use the org-agenda mainly,
;; I hope someone continue integration with the org.)

;; (require 'calfw-org)
;;
;; ;; use org agenda buffer style keybinding.
;; ;; (setq calfw-org-overwrite-default-keybinding t)
;;
;; M-x calfw-org-open-calendar

;;; Code:

(require 'calfw)
(require 'org)
(require 'org-agenda)
(require 'org-element)
(require 'org-capture)
(require 'google-maps nil t)

(defgroup calfw-org nil
  "Options about calfw-org."
  :tag "Calfw Org"
  :group 'org
  :group 'calfw)

(defcustom calfw-org-capture-template nil
  "`org-capture' template to use with `calfw'.

If you use `org-capture' with `calfw', you should set this variable.
For example:
\\='(\\\"c\\\" \\\"calfw2org\\\" entry (file nil)  \\\"* %?\\n %(calfw-org-capture-day)\\\")"
  :group 'calfw-org
  :version "24.1"
  :type
  '(list string string symbol (list symbol (choice file (const nil))) string))

(defsubst calfw-org--tp (text prop)
  "Return text property PROP at position 0 in TEXT."
  (get-text-property 0 prop text))

(defvar calfw-org-agenda-schedule-args nil
  "Default arguments for collecting agenda entries.
If value is nil, `org-agenda-entry-types' is used.")

(defvar calfw-org-overwrite-default-keybinding nil
  "Overwrite default keybinding.

 needs Emacs restart if it does not work.

For example:
------------------------------------------------
key | function
------------------------------------------------
g   | `calfw-refresh-calendar-buffer'
j   | `calfw-org-goto-date'
k   | `org-capture'
x   | `calfw-org-clean-exit'
d   | `calfw-change-view-day'
v d | `calfw-change-view-day'
v w | `calfw-change-view-week'
v m | `calfw-change-view-month'
------------------------------------------------")

(defvar calfw-org-face-agenda-item-foreground-color "Seagreen4"
  "Variable for org agenda item foreground color.")

(defun calfw-org--collect-schedules-period (org-files begin end)
  "Return org schedule items between BEGIN and END from ORG-FILES."
  (let ((org-agenda-prefix-format " "))
    (let ((org-agenda-buffer
           (when (buffer-live-p org-agenda-buffer)
             org-agenda-buffer)))
      (org-compile-prefix-format nil))

    ;; We need several hacks to resolve issues like those in #91.
    ;;
    ;; Org diary for schedules/deadline is weird. It returns the first date an
    ;; item is scheduled, then does not return any that were scheduled before
    ;; today but after that first date (and no real way to control "today").
    ;; To resolve, we employ an incomplete "feature" using
    ;; `org-extend-today-until' to redefine that "today" is one second before
    ;; the start of the current date.

    ;; Then it returns all future events if `org-agenda-show-future-repeats'
    ;; is non-nil, but the `date' property is set to the date of the first
    ;; scheduled date if it is in the past relative to today. To resolve this,
    ;; we instead update the value of `date' to be our input.
    (let ((now (float-time (current-time))))
      (cl-loop
       for date in (calfw-enumerate-days begin end)
       for target = (float-time (calfw-calendar-to-emacs date))
       for org-extend-today-until = (max 0 (/ (- now target -1) 3600))
       append
       (cl-loop
        for file in org-files
        append
        (progn
          (org-check-agenda-file file)
          ;; We set the date here, since we cannot rely on the
          ;; existing `date' property for schedules and
          ;; deadlines.
          (mapcar (lambda (entry) (calfw--tp entry 'date date))
                  (apply #'org-agenda-get-day-entries
                         file date
                         calfw-org-agenda-schedule-args))))))))

(defun calfw-org-onclick ()
  "Jump to the clicked org item."
  (interactive)
  (let ((marker (get-text-property (point) 'org-marker))
        (link   (get-text-property (point) 'org-link))
        (file   (get-text-property (point) 'cfw:org-file))
        (beg    (get-text-property (point) 'cfw:org-h-beg))
        ;; (loc    (get-text-property (point) 'cfw:org-loc))
        )
    (when link
      (org-link-open-from-string link))
    (when (and marker (marker-buffer marker))
      (org-mark-ring-push)
      (switch-to-buffer (marker-buffer marker))
      (widen)
      (goto-char (marker-position marker))
      (when (derived-mode-p 'org-mode)
        (org-reveal)))
    (when beg
      (find-file file)
      (goto-char beg)
      (org-cycle))))

(defun calfw-org-jump-map ()
  "Jump to the clicked org item."
  (interactive)
  (when (fboundp 'google-maps) ;; TODO: Not sure where this function is from!
    (let ((loc    (get-text-property (point) 'cfw:org-loc)))
      (when loc
        (google-maps loc)))))

(defun calfw-org-clean-exit ()
  "Close buffers opened by calfw-org before closing Calendar Framework."
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (bury-buffer))

(defvar calfw-org-text-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'calfw-org-onclick)
    (define-key map (kbd "RET") 'calfw-org-onclick)
    (define-key map (kbd "C-c C-o") 'calfw-org-onclick)
    (define-key map (kbd "m") 'calfw-org-jump-map)
    map)
  "Key map on the calendar item text.")

(defun calfw-org--extract-summary (org-item)
  "Remove strings from ORG-ITEM."
  (let* ((item org-item) (tags (calfw-org--tp item 'tags)))
    ;; (when (string-match calfw-org-todo-keywords-regexp item) ; dynamic bind
    ;;   (setq item (replace-match "" nil nil item)))
    (if tags
        (when (string-match (concat "[\t ]*:+" (mapconcat #'identity tags ":+") ":+[\t ]*$") item)
          (setq item (replace-match "" nil nil item))))
    (when (string-match "[0-9]\\{2\\}:[0-9]\\{2\\}\\(-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?[\t ]+" item)
      (setq item (replace-match "" nil nil item)))
    (when (string-match "^ +" item)
      (setq item (replace-match "" nil nil item)))
    (when (= 0 (length item))
      (setq item (calfw-org--tp org-item 'org-category)))
    item))

(defun calfw-org-summary-format (item)
  "Format an ITEM for display.

ITEM is an org entry.  Return a string with text properties."
  (let* (;; (time (calfw-org--tp item 'time))
         (time-of-day (calfw-org--tp item 'time-of-day))
         (time-str (and time-of-day
                        (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
         ;; (category (calfw-org--tp item 'org-category))
         ;; (tags (calfw-org--tp item 'tags))
         ;; (marker (calfw-org--tp item 'org-marker))
         ;; (buffer (and marker (marker-buffer marker)))
         (text (calfw-org--extract-summary item))
         (props (calfw--extract-text-props item 'face 'keymap))
         (extra (calfw-org--tp item 'extra)))
    (setq text (substring-no-properties text))
    (when (and extra (string-match (concat "^" org-deadline-string ".*") extra))
      (add-text-properties 0 (length text) (list 'face
                                                 (org-agenda-deadline-face 1.0))
                           text))
    (if org-todo-keywords-for-agenda
        (when (string-match (concat "^[\t ]*\\<\\(" (mapconcat #'identity org-todo-keywords-for-agenda "\\|") "\\)\\>") text)
          (add-text-properties (match-beginning 1) (match-end 1) (list 'face (org-get-todo-face (match-string 1 text))) text)))
    ;;; ------------------------------------------------------------------------
    ;;; act for org link
    ;;; ------------------------------------------------------------------------
    (setq text (replace-regexp-in-string "%[0-9A-F]\\{2\\}" " " text))
    (if (string-match org-link-bracket-re text)
        (let* ((desc (if (match-end 2) (match-string-no-properties 2 text)))
               (link (org-link-unescape (match-string-no-properties 1 text)))
               (help (concat "LINK: " link))
               (link-props (list
                            'face 'org-link
                            'mouse-face 'highlight
                            'help-echo help
                            'org-link link)))
          (if desc
              (progn
                (setq desc (apply #'propertize desc link-props))
                (setq text (replace-match desc nil nil text)))
            (setq link (apply #'propertize link link-props))
            (setq text (replace-match link nil nil text)))))
    (when time-str
      (setq text (concat time-str text)))
    (propertize
     (apply #'propertize text props)
     ;; include org filename
     ;; (and buffer (concat " " (buffer-name buffer)))
     'keymap calfw-org-text-keymap)))

(defvar calfw-org-schedule-summary-transformer 'calfw-org-summary-format
  "Transforms the org item string to calendar title.
If this function splits into a list of string, the calfw displays
those string in multi-lines.")

(defun calfw-org-normalize-date (date)
  "Return a normalized date (month day year) from DATE."
  (cond
   ((numberp date)
    (calendar-gregorian-from-absolute date))
   (t date)))

(defun calfw-org-get-timerange (text &optional at-begin)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil.
If AT-BEGIN is non-nil, then it should be the date of the first
day in the calendar."
  (let* ((dotime (calfw-org--tp text 'dotime))
         date-string)
    (and (or (and (stringp dotime)
                  (string-match org-ts-regexp dotime)
                  (setq date-string (match-string 1 dotime)))
             at-begin)
	 (let ((extra (calfw-org--tp text 'extra)))
	   (when (and extra (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra))
	     (let* ((cur-day (string-to-number
			      (match-string 1 extra)))
		    (total-days (string-to-number
				 (match-string 2 extra)))
                    (date (if date-string
                              (org-read-date nil t date-string)
                            (calfw-calendar-to-emacs at-begin)))
		    (start-date (time-subtract
				 date
                                 ;; Add an extra second to ensure robustness
				 (seconds-to-time (1- (* 3600 24 (- cur-day 1))))))
		    (end-date (time-add
			       date
			       (seconds-to-time (* 3600 24 (- total-days cur-day))))))
	       (list (calendar-gregorian-from-absolute (time-to-days start-date))
		     (calendar-gregorian-from-absolute
                      (time-to-days end-date))
                     text)))))))

(defun calfw-org--schedule-period-to-calendar (org-files begin end)
  "Return calfw calendar items between BEGIN and END from ORG-FILES."
  (cl-loop
   ;;with calfw-org-todo-keywords-regexp = (regexp-opt org-todo-keywords-for-agenda) ; dynamic bind
   with contents = nil with periods = nil
   for i in (calfw-org--collect-schedules-period org-files begin end)
   for date = (calfw-org--tp i 'date)
   for line = (funcall calfw-org-schedule-summary-transformer i)
   for range = (calfw-org-get-timerange line (and (equal date begin) date))
   if range
   do
   (unless (member range periods)
     (push range periods))
   else do
   ;; dotime is not present if this event was already added as a timerange
   (when (calfw-org--tp i 'dotime)
     (setq contents (calfw--contents-add
		     (calfw-org-normalize-date date)
		     line contents)))
   finally return (nconc contents (list (cons 'periods periods)))))

(defun calfw-org--schedule-sorter (text1 text2)
  "Compare org schedule items TEXT1 and TEXT2."
  (condition-case _
      (let ((time1 (calfw-org--tp text1 'time-of-day))
            (time2 (calfw-org--tp text2 'time-of-day)))
        (cond
         ((and time1 time2) (< time1 time2))
         (time1 t)   ; time object is moved to upper
         (time2 nil) ;
         (t (string-lessp text1 text2))))
    (error (string-lessp text1 text2))))

(defun calfw-org--schedule-sorter2 (text1 text2)
  "Compare org schedule items TEXT1 and TEXT2."
  (condition-case _
      (let ((time1 (calfw-org--tp text1 'time-of-day))
            (time2 (calfw-org--tp text2 'time-of-day)))
        (cond
         ((and time1 time2) (< time1 time2))
         (time1 nil) ; time object is moved to upper
         (time2 t)   ;
         (t (string-lessp text1 text2))))
    (error (string-lessp text1 text2))))

(defun calfw-org-format-title (file h-obj t-obj h-beg loc)
  "Create a text string for the title of the headline H-OBJ.

Create a text string for the title of the headline H-OBJ in FILE
at H-BEG and LOC, using time information from T-OBJ.  Return a
string with `keymap', `display', `cfw:org-file', `cfw:org-h-beg',
and `cfw:org-loc' properties."
  (propertize
   (concat
    (when  (org-element-property :hour-start t-obj)
      (format "%02i:%02i "
              (org-element-property :hour-start t-obj)
              (org-element-property :minute-start t-obj)))
    (org-element-property :title h-obj))
   'keymap calfw-org-text-keymap
   'cfw:org-file file
   'cfw:org-h-beg h-beg
   'cfw:org-loc loc))

(defun calfw-org-format-date (t-obj lst)
  "Format a date object T-OBJ using a list of properties LST.
Return a list of formatted properties."
  (mapcar
   (lambda (v)
     (org-element-property v t-obj)) lst))

(defun calfw-org-filter-datetime (t-obj lst)
  "Return the datetime object (T-OBJ) formatted according to LST if it is not nil."
  (if (car (calfw-org-format-date t-obj lst))
      (calfw-org-format-date t-obj lst)
    nil))

(defun calfw-org-convert-event (file h-obj t-obj h-beg)
  "Create a calfw event from the org headline object H-OBJ in FILE.

Create the event using time object T-OBJ and the beginning of
headline object H-BEG.  Returns the calfw event created."
  (let ((sdate '(:month-start :day-start :year-start))
        (stime '(:hour-start :minute-start))
        (edate '(:month-end :day-end :year-end))
        (etime '(:hour-end :minute-end))
        (loc (org-element-property :LOCATION h-obj)))
    (make-calfw-event
     :start-date  (calfw-org-format-date t-obj sdate)
     :start-time  (calfw-org-filter-datetime t-obj stime)
     :end-date    (calfw-org-filter-datetime t-obj edate)
     :end-time    (calfw-org-filter-datetime t-obj etime)
     :title       (calfw-org-format-title file h-obj t-obj h-beg loc)
     :location    loc
     :description (if (org-element-property :contents-begin h-obj)
                      (replace-regexp-in-string
                       " *:PROPERTIES:\n  \\(.*\\(?:\n.*\\)*?\\) :END:\n" ""
                       (buffer-substring (org-element-property :contents-begin h-obj)
                                         (org-element-property :contents-end h-obj)))
                    nil))))

(defun calfw-org-convert-org-to-calfw (file)
  "Convert org entries in FILE to calfw format.

Returns an alist of `:periods' and `:contents'."
  (save-excursion
    (with-current-buffer
        (find-file-noselect file)
      (let*
          ((elem-obj (org-element-parse-buffer))
           (pos-lst `(,@(org-element-map elem-obj 'timestamp
                          (lambda (hl) (org-element-property :begin hl)))
                      ,@(org-element-map (org-element-map elem-obj 'headline
                                           (lambda (hl)
                                             (org-element-property
                                              :deadline hl)))
                            'timestamp
                          (lambda (hl) (org-element-property :begin hl)))
                      ,@(org-element-map (org-element-map elem-obj 'headline
                                           (lambda (hl)
                                             (org-element-property :scheduled hl)))
                            'timestamp
                          (lambda (hl) (org-element-property :begin hl))))))
        (cl-loop for pos in pos-lst
                 do (goto-char pos)
                 for t-obj =  (org-element-timestamp-parser)
                 for h-obj = (progn
                               (org-back-to-heading t)
                               (org-element-headline-parser (point-max) t))
                 for h-beg  = (point)
                 for event = (calfw-org-convert-event file h-obj t-obj h-beg)
                 for ts-type = (org-element-property :type t-obj)
                 if (eq 'active-range ts-type)
                 collect event into periods
                 else if (eq 'active ts-type)
                 collect event into contents
                 ;; else do
                 ;; (message "calfw-org: Cannot handle event")
                 finally
                 (kill-buffer (get-file-buffer file))
                 (cl-return `((periods ,periods) ,@contents)))))))

(defun calfw-org-to-calendar (file begin end)
  "Convert org entries in FILE between BEGIN and END to calfw events."
  (cl-loop for event in (calfw-org-convert-org-to-calfw file)
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

(defun calfw-org-create-file-source (name file color)
  "Create an org-element based source with NAME, FILE, and COLOR."
  (make-calfw-source
   :name (concat "Org:" name)
   :color color
   :data (lambda (begin end)
           (calfw-org-to-calendar file begin end))))

(defun calfw-org-capture-day ()
  "Return a string representing the date at the cursor position."
  (with-current-buffer  (get-buffer-create calfw-calendar-buffer-name)
    (let ((pos (calfw-cursor-to-nearest-date)))
      (concat "<"
              (format-time-string  "%F %a"
                                   (encode-time 0 0 0
                                                (calendar-extract-day pos)
                                                (calendar-extract-month pos)
                                                (calendar-extract-year pos)))
              ">"))))

(when calfw-org-capture-template
  (setq org-capture-templates
        (append org-capture-templates (list calfw-org-capture-template))))

(defun calfw-org-capture ()
  "Open the `org-agenda' buffer on the selected date.

If `calfw-org-capture-template' is set, use `org-capture' with
the template specified by the CAR of
`calfw-org-capture-template'.  Otherwise, display a message
indicating that `calfw-org-capture-template' is not set."
  (interactive)
  (if calfw-org-capture-template
      (org-capture nil (car calfw-org-capture-template))
    (message "calfw-org-capture-template is not set yet.")))

(defun calfw-org-open-agenda-day ()
  "Open `org-agenda' buffer on the selected date.

Open the `org-agenda' buffer for the date at point, DATE."
  (interactive)
  (let ((date (calfw-cursor-to-nearest-date)))
    (when date
      (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))))

;; TODO: Need to find a better place to do org-specific mapping
(define-key
 calfw-calendar-mode-map "c" 'calfw-org-capture)

(defvar calfw-org-schedule-map
  (calfw--define-keymap
   '(("q"   . bury-buffer)
     ("SPC" . calfw-org-open-agenda-day)))
  "Key map for the calendar buffer.")

(defvar calfw-org-custom-map
  (calfw--define-keymap
   '(("g"   . calfw-refresh-calendar-buffer)
     ("j"   . calfw-org-goto-date)
     ("k"   . org-capture)
     ("q"   . bury-buffer)
     ("d"   . calfw-change-view-day)
     ("v d" . calfw-change-view-day)
     ("v w" . calfw-change-view-week)
     ("v m" . calfw-change-view-month)
     ("x"   . calfw-org-clean-exit)
     ("SPC" . calfw-org-open-agenda-day)))
  "Key map for the calendar buffer.")

(defun calfw-org-create-source (name org-files color)
  "Create a calfw source named NAME with org files ORG-FILES and COLOR."
  (make-calfw-source
   :name name :color color
   :data (apply-partially #'calfw-org--schedule-period-to-calendar org-files)))

(cl-defun calfw-org-open-calendar (&optional name org-files)
  "Open an org schedule calendar in a new buffer.

Events are taken from ORG-FILES \\=(defaults to those returned by
`org-agenda-files') and NAME is the name of the calendar
\\=(defaults to \"org-agenda\")."
  (interactive)
  (save-excursion
    (let* ((source1 (calfw-org-create-source
                     (or name "org-agenda")
                     (or org-files
                         (org-agenda-files nil 'ifmode))
                     calfw-org-face-agenda-item-foreground-color))
           (curr-keymap (if calfw-org-overwrite-default-keybinding
                            calfw-org-custom-map
                          calfw-org-schedule-map))
           (cp (calfw-create-calendar-component-buffer
                :view 'month
                :contents-sources (list source1)
                :custom-map curr-keymap
                :sorter 'calfw-org--schedule-sorter)))
      (switch-to-buffer (calfw-cp-get-buffer cp))
      (when (not org-todo-keywords-for-agenda)
        (message "Warning: open org-agenda buffer first.")))))

;; (defun calfw-org-from-calendar ()
;;   "Do something. This command should be executed on the calfw calendar."
;;   (interactive)
;;   (let* ((mdy (calfw-cursor-to-nearest-date))
;;          (m (calendar-extract-month mdy))
;;          (d (calendar-extract-day   mdy))
;;          (y (calendar-extract-year  mdy)))
;;     ;; exec org-remember here?
;;     ))

(defun calfw-org-read-date-command ()
  "Read a date and return it as a calendar date value."
  (interactive)
  (calfw-emacs-to-calendar (org-read-date nil 'to-time)))

(defun calfw-org-goto-date ()
  "Move the cursor to the specified date."
  (interactive)
  (calfw-navi-goto-date
   (calfw-org-read-date-command)))

;; (progn (eval-current-buffer) (calfw-open-org-calendar))
;; (setq org-agenda-files '("./org-samples/complex.org"))


(provide 'calfw-org)
;;; calfw-org.el ends here
