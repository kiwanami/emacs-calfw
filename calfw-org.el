;;; calfw-org.el --- calendar view for org-agenda

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar

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

(defvar cfw:org-agenda-schedule-args nil
  "Default arguments for collecting agenda entries.")

(defun cfw:org-collect-schedules-period (begin end)
  "[internal] Return org schedule items between BEGIN and END."
  (let ((org-agenda-prefix-format "")
        (span 'day))
    (org-compile-prefix-format nil)
    (loop for date in (cfw:enumerate-days begin end) append
          (loop for file in (org-agenda-files nil 'ifmode) append
                (progn
                  (org-check-agenda-file file)
                  (apply 'org-agenda-get-day-entries 
                         file date 
                         cfw:org-agenda-schedule-args))))))

(defun cfw:org-onclick ()
  "Jump to the clicked org item."
  (interactive)
  (let ((marker (get-text-property (point) 'org-marker)))
    (when marker
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))))

(defvar cfw:org-text-keymap 
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'cfw:org-onclick)
    (define-key map (kbd "<return>") 'cfw:org-onclick)
    map)
  "key map on the calendar item text.")

(defun cfw:org-summary-format (item)
  "Format an item. (How should be displayed?)"
  (let* ((time (get-text-property 0 'time item))
         (time-of-day (get-text-property 0 'time-of-day item))
         (time-str (and time-of-day 
                        (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
         (category (get-text-property 0 'org-category item))
         (tags (get-text-property 0 'tags item))
         (marker (get-text-property 0 'org-marker item))
         (buffer (marker-buffer marker)))
    (propertize
     (concat time-str item " " (buffer-name buffer))
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

(defun cfw:org-schedule-period-to-calendar (begin end)
  "[internal] Return calfw calendar items between BEGIN and END
from the org schedule data."
  (loop 
   for i in (cfw:org-collect-schedules-period begin end)
        for date = (get-text-property 0 'date i)
        for line = (funcall cfw:org-schedule-summary-transformer i)
        with contents = nil
        do 
        (setq contents (cfw:contents-add (cfw:org-normalize-date date)
                                         line contents))
        finally return contents))

(defun cfw:org-schedule-sorter (text1 text2)
  "[internal] Sorting algorithm for org schedule items.
TEXT1 < TEXT2."
  (condition-case err
      (let ((time1 (get-text-property 0 'time-of-day text1))
            (time2 (get-text-property 0 'time-of-day text2)))
        (cond
         ((and time1 time2)
          (< time1 time2))
         (time1 t)   ; time object is moved to upper
         (time2 nil) ; 
         (t
          (string-lessp text1 text2))))
    (error (string-lessp text1 text2))))

(defvar cfw:org-schedule-map
  (cfw:define-keymap
   '(
     ("q"   . kill-buffer)
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

;; (progn (eval-current-buffer) (cfw:open-org-calendar))
;; (setq org-agenda-files '("a.org"))


(provide 'calfw-org)
;;; calfw-org.el ends here
