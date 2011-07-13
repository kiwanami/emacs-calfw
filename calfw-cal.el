;;; calfw-cal.el --- calendar view for org-agenda

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
(require 'calendar)

(defvar cfw:cal-agenda-schedule-args nil
  "Default arguments for collecting agenda entries.")

(defun cfw:cal-collect-schedules-period (begin end)
  "[internal] Return diary schedule items between BEGIN and END."
  (loop for date in (cfw:enumerate-days begin end) append
        (apply 'diary-list-entries date '1
               cfw:cal-agenda-schedule-args)))

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

(defun cfw:cal-schedule-period-to-calendar (begin end)
  "[internal] Return calfw calendar items between BEGIN and END
from the diary schedule data."
  (loop
   for i in (cfw:cal-collect-schedules-period begin end)
   for date = (car i)
   for line = (car (cdr i))
   with contents = nil
   do
   (message line)
   (setq contents (cfw:contents-add date line contents))
   finally return contents))

(defvar cfw:cal-schedule-map
  (cfw:define-keymap
   '(
     ("q" . kill-buffer)
     ))
  "Key map for the calendar buffer.")

(defun cfw:cal-create-source (&optional color)
  "Create calendar source."
  (make-cfw:source
   :name "calendar diary"
   :color (or color "Seagreen4")
   :data 'cfw:cal-schedule-period-to-calendar))

(defun cfw:open-diary-calendar ()
  "Open the diary schedule calendar in the new buffer."
  (interactive)
  (let* ((source1 (cfw:cal-create-source))
         (cp (cfw:create-calendar-component-buffer
              :view 'month
              :contents-sources (list source1))))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

(defun cfw:cal-from-calendar ()
  "Do something. This command should be executed on the calfw calendar."
  (interactive)
  (let* ((mdy (cfw:cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy)))
    ;; exec something here?
    ))

;; (progn (eval-current-buffer) (cfw:open-diary-calendar))

(provide 'calfw-cal)
;;; calfw-cal.el ends here
