;;; calfw-howm.el --- calendar view for howm

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

;; (eval-after-load "howm-menu" '(progn
;;   (require 'calfw-howm)
;;   (cfw:install-howm-schedules)
;;   (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar)
;; ))

;; If you are using Elscreen, here is useful.
;; (define-key howm-mode-map (kbd "M-C") 'cfw:elscreen-open-howm-calendar)

;; One can open a standalone calendar buffer by
;; M-x cfw:open-howm-calendar

;; You can display a calendar in your howm menu.
;; %here%(cfw:howm-schedule-inline)

;;; Code:

(require 'howm)
(require 'calfw)

(defvar cfw:howm-schedule-cache nil "[internal] Cache data for schedule items of howm.")

(defun cfw:howm-schedule-cache-clear ()
  "clear cache for howm schedule items."
  (setq cfw:howm-schedule-cache nil))

(defvar cfw:howm-schedule-hook nil
  "Hook which is called after retrieval of howm schedule items.")

(defun cfw:howm-schedule-get ()
  "[internal] Return all schedule items in the whole howm data. If cache
data exists, this function uses the cache."
  (unless cfw:howm-schedule-cache
    (let* ((howm-schedule-types howm-schedule-menu-types)
           (raw (howm-reminder-search howm-schedule-types)))
      (setq cfw:howm-schedule-cache (howm-schedule-sort-items raw)))
    (run-hooks 'cfw:howm-schedule-hook))
  cfw:howm-schedule-cache)

(defun cfw:to-howm-date (date)
  "[internal] Convert a date format from the Emacs calendar list
to the number of howm encoded days."
  (apply 'howm-encode-day
         (mapcar 'number-to-string
                 (list (calendar-extract-day date)
                       (calendar-extract-month date)
                       (calendar-extract-year date)))))

(defun cfw:howm-schedule-period (begin end)
  "[internal] Return howm schedule items between BEGIN and END."
  (let* ((from (cfw:to-howm-date begin))
         (to (cfw:to-howm-date end))
         (filtered (howm-cl-remove-if
                    (lambda (item)
                      (let ((s (howm-schedule-date item)))
                        (or (< s from)
                            (< to s))))
                    (cfw:howm-schedule-get))))
    (howm-schedule-sort-items filtered)))

(defvar cfw:howm-schedule-summary-transformer
  (lambda (line) line)
  "Transformation function which transforms the howm summary string to calendar title.
If this function splits into a list of string, the calfw displays those string in multi-lines.")

(defun cfw:howm-schedule-parse-line (line)
  "[internal] Parse the given string and return a result list, (date num type summary)."
  (when (string-match "^\\[\\([^@!]+\\)\\]\\([@!]\\)\\([0-9]*\\) \\(.*\\)$" line)
    (list
     (match-string 1 line) (string-to-number (match-string 3 line))
     (match-string 2 line) (match-string 4 line))))

(defun cfw:howm-schedule-period-to-calendar (begin end)
  "[internal] Return calfw calendar items between BEGIN and END
from the howm schedule data."
  (loop with contents = nil
        with periods = nil
        for i in (cfw:howm-schedule-period begin end)
        for date = (cfw:emacs-to-calendar
                    (seconds-to-time (+ 10 (* (howm-schedule-date i) 24 3600))))
        for (datestr num type summary) = (cfw:howm-schedule-parse-line (howm-item-summary i))
        for summary = (funcall cfw:howm-schedule-summary-transformer summary)
        do
        (cond
         ((< 0 num)
          (push (list date (cfw:date-after date (1- num)) summary) periods))
         (t
          (setq contents (cfw:contents-add date summary contents))))
        finally return (nconc contents (list (cons 'periods periods)))))

(defun cfw:howm-create-source (&optional color)
  "Create a howm source."
  (make-cfw:source
   :name "howm schedule"
   :color (or color "SteelBlue")
   :update 'cfw:howm-schedule-cache-clear
   :data 'cfw:howm-schedule-period-to-calendar))

(defvar cfw:howm-schedule-map
  (cfw:define-keymap
   '(
     ("RET" . cfw:howm-from-calendar)
     ("q"   . kill-buffer)
     ))
  "Key map for the howm calendar mode.")

(defvar cfw:howm-schedule-contents nil  "A list of cfw:source objects for schedule contents.")
(defvar cfw:howm-annotation-contents nil  "A list of cfw:source objects for annotations.")

(defun cfw:open-howm-calendar ()
  "Open a howm schedule calendar in the new buffer."
  (interactive)
  (let ((cp (cfw:create-calendar-component-buffer
             :custom-map cfw:howm-schedule-map
             :view 'month
             :contents-sources (append (list (cfw:howm-create-source))
                                       cfw:howm-schedule-contents)
             :annotation-sources cfw:howm-annotation-contents)))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

(defun cfw:howm-from-calendar ()
  "Display a howm schedule summary of the date on the cursor,
searching from the whole howm data.
This command should be executed on the calfw calendar."
  (interactive)
  (let* ((mdy (cfw:cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y))))
    (howm-keyword-search key)))

(defun cfw:howm-from-calendar-fast ()
  "Display a howm schedule summary of the date on the cursor,
searching from the cache. So, this command is faster than `cfw:howm-from-calendar'.
This command should be executed on the calfw calendar."
  (interactive)
  (let* ((mdy (cfw:cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y)))
         (items (cfw:howm-schedule-period mdy mdy)))
    (cond
     ((= 1 (length items))
      (howm-view-open-item (car items)))
     (t
      (howm-view-summary (format "Schedules : %s" (cfw:strtime mdy))
                         items (list key))
      (howm-view-summary-check t)))))

;; (define-key cfw:howm-schedule-map (kbd "RET") 'cfw:howm-from-calendar-fast)
;; (define-key cfw:howm-schedule-inline-keymap (kbd "RET") 'cfw:howm-from-calendar-fast)

;;; Region

(defvar cfw:howm-schedule-inline-keymap
  (cfw:define-keymap
   '(("RET" . cfw:howm-from-calendar)))
  "Key map for the howm inline calendar.")

(defun cfw:howm-schedule-inline (&optional width height view)
  "Inline function for the howm menu. See the comment text on the top of this file for the usage."
  (let ((custom-map (copy-keymap cfw:howm-schedule-inline-keymap)) cp)
    (set-keymap-parent custom-map cfw:calendar-mode-map)
    (setq cp (cfw:create-calendar-component-region
              :width width :height (or height 10)
              :keymap custom-map
              :contents-sources (append (list (cfw:howm-create-source))
                                        cfw:howm-schedule-contents)
              :annotation-sources cfw:howm-annotation-contents
              :view (or view 'month))))
  "") ; for null output

;;; Installation

(defun cfw:install-howm-schedules ()
  "Add a schedule collection function to the calfw for the howm
schedule data and set up inline calendar function for the howm menu."
  (interactive)
  (add-hook 'howm-after-save-hook 'cfw:howm-schedule-cache-clear)
  (add-to-list 'howm-menu-allow 'cfw:howm-schedule-inline))

;;; for Elscreen

(eval-after-load "elscreen-howm"
  '(progn
     (defun cfw:elscreen-open-howm-calendar ()
       "Open the calendar in the new screen."
       (interactive)
       (save-current-buffer
         (elscreen-create))
       (cfw:open-howm-calendar))

     (defun cfw:elscreen-kill-calendar ()
       "Kill the calendar buffer and the screen."
       (interactive)
       (kill-buffer nil)
       (unless (elscreen-one-screen-p)
         (elscreen-kill)))

     (define-key cfw:howm-schedule-map (kbd "q") 'cfw:elscreen-kill-calendar)
     ))

(provide 'calfw-howm)
;;; calfw-howm.el ends here
