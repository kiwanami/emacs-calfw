;;; calfw-org.el --- calendar view for org-agenda     -*- coding: utf-8 -*-

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar, org

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
;; M-x cfw:open-org-calendar

;;; Code:

(require 'calfw)
(require 'org)
(require 'org-agenda)
(require 'org-element)
(require 'org-capture)

(defgroup cfw-org nil
  "Options about calfw-org."
  :tag "Calfw Org"
  :group 'org
  :group 'cfw)

(defcustom cfw:org-capture-template
  '("c" "calfw2org" entry (file nil)  "* %?\n %(cfw:org-capture-day)")
  "org-capture template."
  :group 'cfw-org
  :version "24.1"
  :type
  '(list string string symbol (list symbol (choice file (const nil))) string))

(defsubst cfw:org-tp (text prop)
  "[internal] Return text property at position 0."
  (get-text-property 0 prop text))

(defvar cfw:org-agenda-schedule-args nil
  "Default arguments for collecting agenda entries.")

(defvar cfw:org-icalendars nil
  "Org buffers for exporting icalendars.
Setting a list of the custom agenda files, one can use the
different agenda files from the default agenda ones.")

(defun cfw:org-collect-schedules-period (begin end)
  "[internal] Return org schedule items between BEGIN and END."
  (let ((org-agenda-prefix-format " ")
        (span 'day))
    (setq org-agenda-buffer
      (when (buffer-live-p org-agenda-buffer)
        org-agenda-buffer))
    (org-compile-prefix-format nil)
    (loop for date in (cfw:enumerate-days begin end) append
          (loop for file in (or cfw:org-icalendars (org-agenda-files nil 'ifmode))
                append
                (progn
                  (org-check-agenda-file file)
                  (apply 'org-agenda-get-day-entries
                         file date
                         cfw:org-agenda-schedule-args))))))

(defun cfw:org-onclick ()
  "Jump to the clicked org item."
  (interactive)
  (let (
    (marker (get-text-property (point) 'org-marker))
    (link   (get-text-property (point) 'org-link))
    (file   (get-text-property (point) 'cfw:org-file))
    (beg    (get-text-property (point) 'cfw:org-h-beg)))
    (when link
      (org-open-link-from-string link))
    (when (and marker (marker-buffer marker))
      (org-mark-ring-push)
      (switch-to-buffer (marker-buffer marker))
      (widen)
      (goto-char (marker-position marker))
      (when (eq major-mode 'org-mode)
        (org-reveal)))
    (when beg
      (find-file file)
      (goto-char beg)
      (org-cycle))))

(defun cfw:org-clean-exit ()
  "Close buffers opened by calfw-org before closing Calendar Framework."
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (bury-buffer))

(defvar cfw:org-text-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'cfw:org-onclick)
    (define-key map (kbd "RET") 'cfw:org-onclick)
    (define-key map (kbd "C-c C-o") 'cfw:org-onclick)
    map)
  "key map on the calendar item text.")

(defun cfw:org-extract-summary (org-item)
  "[internal] Remove some strings."
  (let ((item org-item))
    ;; (when (string-match cfw:org-todo-keywords-regexp item) ; dynamic bind
    ;;   (setq item (replace-match "" nil nil item)))
    (when (string-match "^ +" item)
      (setq item (replace-match "" nil nil item)))
    (when (= 0 (length item))
      (setq item (cfw:org-tp org-item 'org-category)))
    item))

(defun cfw:org-summary-format (item)
  "Format an item. (How should be displayed?)"
  (let* ((time (cfw:org-tp item 'time))
         (time-of-day (cfw:org-tp item 'time-of-day))
         (time-str (and time-of-day
                        (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
         (category (cfw:org-tp item 'org-category))
         (tags (cfw:org-tp item 'tags))
         (marker (cfw:org-tp item 'org-marker))
         (buffer (and marker (marker-buffer marker)))
         (text (cfw:org-extract-summary item))
         (props (cfw:extract-text-props item 'face 'keymap))
         (extra (cfw:org-tp item 'extra))
         (i 0))
    ;;; ------------------------------------------------------------------------
    ;;; set title color as default agenda color
    ;;; ------------------------------------------------------------------------
    (setq text (if (string-match "^Deadline:.*" extra)
        text
        (apply 'propertize (substring-no-properties text) props)))
    ;;; ------------------------------------------------------------------------
    ;;; skip time related string
    ;;; ------------------------------------------------------------------------
    (setq text (replace-regexp-in-string "[0-9]\\{2\\}:[0-9]\\{2\\}\\(-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?[\t ]+" "" text))
    ;;; ------------------------------------------------------------------------
    ;;; skip tags
    ;;; ------------------------------------------------------------------------
    (if tags
      (progn
        (while (< i (length tags))
          (setq text (replace-regexp-in-string (concat ":" (elt tags i) ":") "" text))
          (setq i (+ i 1)))
        (setq text (replace-regexp-in-string "[\t ]*:?$" "" text))))
    ;;; ------------------------------------------------------------------------
    ;;; act for org link
    ;;; ------------------------------------------------------------------------
    (setq text (replace-regexp-in-string "%[0-9A-F]\\{2\\}" " " text))
    (if (string-match org-bracket-link-regexp text)
      (progn
        (setq desc (if (match-end 3) (org-match-string-no-properties 3 text)))
        (setq link (org-link-unescape (org-match-string-no-properties 1 text)))
        (setq help (concat "LINK: " link))
        (setq link-props
           (list
                 'face 'org-link
                 'mouse-face 'highlight
                 'help-echo help
                 'org-link link
                 ))
        (if desc
          (progn
            (setq desc (apply 'propertize desc link-props))
            (setq text (replace-match desc nil nil text)))
          (setq link (apply 'propertize link link-props))
          (setq text (replace-match link nil nil text)))
        ))
    (propertize
      (concat
        (if time-str
          (apply 'propertize time-str props))
        text
        ;; include org filename
        ;; (and buffer (concat " " (buffer-name buffer)))
        )
      'keymap cfw:org-text-keymap
      ;; Delete the display property, since displaying images will break our
      ;; table layout.
      'display nil)))

(defvar cfw:org-schedule-summary-transformer 'cfw:org-summary-format
  "Transformation function which transforms the org item string to calendar title.
If this function splits into a list of string, the calfw displays those string in multi-lines.")

(defun cfw:org-normalize-date (date)
  "Return a normalized date. (MM DD YYYY)."
  (cond
   ((numberp date)
    (calendar-gregorian-from-absolute date))
   (t date)))

(defun cfw:org-get-timerange (text)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil."
  (let* ((dotime (cfw:org-tp text 'dotime))
         (ps (and dotime (stringp dotime) (string-match org-tr-regexp dotime))))
    (and ps
         (let* ((s1 (match-string 1 dotime))
                (s2 (match-string 2 dotime))
                (d1 (time-to-days (org-time-string-to-time s1)))
                (d2 (time-to-days (org-time-string-to-time s2))))
           (list (calendar-gregorian-from-absolute d1)
                 (calendar-gregorian-from-absolute d2) text)))))

(defun cfw:org-schedule-period-to-calendar (begin end)
  "[internal] Return calfw calendar items between BEGIN and END
from the org schedule data."
  (loop
   with cfw:org-todo-keywords-regexp = (regexp-opt org-todo-keywords-for-agenda) ; dynamic bind
   with contents = nil with periods = nil
   for i in (cfw:org-collect-schedules-period begin end)
   for date = (cfw:org-tp i 'date)
   for line = (funcall cfw:org-schedule-summary-transformer i)
   for range = (cfw:org-get-timerange line)
   if range do
   (unless (member range periods)
     (push range periods))
   else do
   (setq contents (cfw:contents-add
                   (cfw:org-normalize-date date)
                   line contents))
   finally return (nconc contents (list (cons 'periods periods)))))

(defun cfw:org-schedule-sorter (text1 text2)
  "[internal] Sorting algorithm for org schedule items.
TEXT1 < TEXT2."
  (condition-case err
      (let ((time1 (cfw:org-tp text1 'time-of-day))
            (time2 (cfw:org-tp text2 'time-of-day)))
        (cond
         ((and time1 time2) (< time1 time2))
         (time1 t)   ; time object is moved to upper
         (time2 nil) ;
         (t (string-lessp text1 text2))))
    (error (string-lessp text1 text2))))

(defun cfw:org-schedule-sorter2 (text1 text2)
  "[internal] Sorting algorithm for org schedule items.
TEXT1 < TEXT2. This function makes no-time items in front of timed-items."
  (condition-case err
      (let ((time1 (cfw:org-tp text1 'time-of-day))
            (time2 (cfw:org-tp text2 'time-of-day)))
        (cond
         ((and time1 time2) (< time1 time2))
         (time1 nil) ; time object is moved to upper
         (time2 t)   ;
         (t (string-lessp text1 text2))))
    (error (string-lessp text1 text2))))

(defun cfw:org-format-title (file h-obj t-obj h-beg)
  (propertize
  (concat
   (when  (org-element-property :hour-start t-obj)
     (format "%02i:%02i "
             (org-element-property :hour-start t-obj)
             (org-element-property :minute-start t-obj)))
   (org-element-property :title h-obj))
  'keymap cfw:org-text-keymap
  'display nil
  'cfw:org-file file
  'cfw:org-h-beg h-beg ))

(defun cfw:org-format-date (t-obj lst)
  (mapcar
   (lambda (v)
     (org-element-property v t-obj)) lst))

(defun cfw:org-filter-datetime (t-obj lst)
  (if (car (cfw:org-format-date t-obj lst))
      (cfw:org-format-date t-obj lst)
    nil))

(defun cfw:org-get-head-beg ()
(save-excursion
  (re-search-backward org-heading-regexp nil t nil)
  (point)))

(defun cfw:org-convert-event (file h-obj t-obj h-beg)
  (let ((sdate '(:month-start :day-start :year-start))
        (stime '(:hour-start :minute-start))
        (edate '(:month-end :day-end :year-end))
        (etime '(:hour-end :minute-end)))
    (make-cfw:event
     :start-date  (cfw:org-format-date t-obj sdate)
     :start-time  (cfw:org-filter-datetime t-obj stime)
     :end-date    (cfw:org-filter-datetime t-obj edate)
     :end-time    (cfw:org-filter-datetime t-obj etime)
     :title       (cfw:org-format-title file h-obj t-obj h-beg)
     :location    (org-element-property :LOCATION h-obj)
     :description (if (org-element-property :contents-begin h-obj)
                      (replace-regexp-in-string
                       " *:PROPERTIES:\n  \\(.*\\(?:\n.*\\)*?\\) :END:\n" ""
                       (buffer-substring (org-element-property :contents-begin h-obj)
                                         (org-element-property :contents-end h-obj)))
                    nil))))

(defun cfw:org-convert-org-to-calfw (file)
  (save-excursion
    (with-current-buffer
        (find-file-noselect file)
      (let*
          ((elem-obj (org-element-parse-buffer))
           (pos-lst `( ,@(org-element-map elem-obj 'timestamp
                           (lambda (hl) (org-element-property :begin hl) ))
                       ,@(org-element-map (org-element-map elem-obj 'headline
                                            (lambda (hl)
                                 (org-element-property :deadline hl) ) ) 'timestamp
                           (lambda (hl) (org-element-property :begin hl) ))
                       ,@(org-element-map (org-element-map elem-obj 'headline
                                            (lambda (hl)
                                              (org-element-property :scheduled hl) ) ) 'timestamp
                           (lambda (hl) (org-element-property :begin hl) )))))
        (loop for pos in pos-lst
              do (goto-char pos)
              for t-obj =  (org-element-timestamp-parser)
              for h-obj = (org-element-headline-parser nil t)
              for h-beg  = (cfw:org-get-head-beg)
              for event = (cfw:org-convert-event file h-obj t-obj h-beg)
              for ts-type = (org-element-property :type t-obj)
              if (eq 'active-range ts-type)
              collect event into periods
              else if (eq 'active ts-type)
              collect event into contents
              ;; else do
              ;; (message "calfw-org: Cannot handle event")
              finally
              (kill-buffer (get-file-buffer file))
              (return `((periods ,periods) ,@contents)))))))

(defun cfw:org-to-calendar (file begin end)
  (loop for event in (cfw:org-convert-org-to-calfw file)
        if (and (listp event)
                (equal 'periods (car event)))
        collect
        (cons
         'periods
         (loop for evt in (cadr event)
               if (and
                   (cfw:date-less-equal-p begin (cfw:event-end-date evt))
                   (cfw:date-less-equal-p (cfw:event-start-date evt) end))
               collect evt))
        else if (cfw:date-between begin end (cfw:event-start-date event))
        collect event))

(defun cfw:org-create-file-source (name file color)
  "Create org-element based source. "
  (lexical-let ((file file))
    (make-cfw:source
     :name (concat "Org:" name)
     :color color
     :data (lambda (begin end)
             (cfw:org-to-calendar file begin end)))))

(defun cfw:org-capture-day ()
  (with-current-buffer  (get-buffer-create cfw:calendar-buffer-name)
    (let ((pos (cfw:cursor-to-nearest-date)))
      (concat "<"
              (format-time-string  "%Y-%m-%d %a"
                                   (encode-time 0 0 0
                                                (calendar-extract-day pos)
                                                (calendar-extract-month pos)
                                                (calendar-extract-year pos)))
              ">"))))

(setq org-capture-templates
      (append org-capture-templates (list cfw:org-capture-template)))

(defun cfw:org-capture ()
  "Open org-agenda buffer on the selected date."
  (interactive)
  (org-capture nil (car cfw:org-capture-template)))

(defun cfw:org-open-agenda-day ()
  "Open org-agenda buffer on the selected date."
  (interactive)
  (let ((date (cfw:cursor-to-nearest-date)))
    (when date
      (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))))

(define-key
  cfw:calendar-mode-map "c" 'cfw:org-capture)

(defvar cfw:org-schedule-map
  (cfw:define-keymap
   '(
     ("q"   . bury-buffer)
     ("SPC" . cfw:org-open-agenda-day)
     ))
  "Key map for the calendar buffer.")

(defun cfw:org-create-source (&optional color)
  "Create org-agenda source."
  (make-cfw:source
   :name "org-agenda"
   :color (or color "Seagreen4")
   :data 'cfw:org-schedule-period-to-calendar))

(defun cfw:open-org-calendar ()
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (let* ((source1 (cfw:org-create-source))
         (cp (cfw:create-calendar-component-buffer
              :view 'month
              :contents-sources (list source1)
              :custom-map cfw:org-schedule-map
              :sorter 'cfw:org-schedule-sorter)))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

(defun cfw:org-from-calendar ()
  "Do something. This command should be executed on the calfw calendar."
  (interactive)
  (let* ((mdy (cfw:cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy)))
    ;; exec org-remember here?
    ))

(defun cfw:org-read-date-command ()
  "Move the cursor to the specified date."
  (interactive)
  (cfw:emacs-to-calendar (org-read-date nil 'to-time)))

;; (progn (eval-current-buffer) (cfw:open-org-calendar))
;; (setq org-agenda-files '("./org-samples/complex.org"))


(provide 'calfw-org)
;;; calfw-org.el ends here
