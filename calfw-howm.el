;;; calfw-howm.el --- Calendar view for howm -*- lexical-binding: t -*-

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Version: 1.7
;; Keywords: calendar
;; Package-Requires: ((emacs "28.1") (calfw "1.7") (howm "1.5.0"))
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

;; (eval-after-load "howm-menu" '(progn
;;   (require 'calfw-howm)
;;   (calfw-howm-install-schedules)
;;   (define-key howm-mode-map (kbd "M-C") 'calfw-howm-open-calendar)
;; ))

;; If you are using Elscreen, here is useful.
;; (define-key howm-mode-map (kbd "M-C") 'calfw-howm-elscreen-open-calendar)

;; One can open a standalone calendar buffer by
;; M-x calfw-howm-open-calendar

;; You can display a calendar in your howm menu.
;; %here%(calfw-howm-schedule-inline)

;;; Code:

(require 'howm)
(require 'calfw)

(defvar calfw-howm-schedule-cache nil
  "[internal] Cache data for schedule items of howm.")

(defun calfw-howm-schedule-cache-clear ()
  "Clear the cache for howm schedule items."
  (setq calfw-howm-schedule-cache nil))

(defvar calfw-howm-schedule-hook nil
  "Hook which is called after retrieval of howm schedule items.")

(defun calfw-howm--schedule-get ()
  "Return all schedule items in the whole howm data.

If cache data exists, this function uses the cache.  Returns
`calfw-howm-schedule-cache'."
  (unless calfw-howm-schedule-cache
    (let* ((howm-schedule-types howm-schedule-menu-types)
           (raw (howm-reminder-search howm-schedule-types)))
      (setq calfw-howm-schedule-cache (howm-schedule-sort-items raw)))
    (run-hooks 'calfw-howm-schedule-hook))
  calfw-howm-schedule-cache)

(defun calfw-howm--convert-date (date)
  "Convert a DATE from the Emacs calendar list to howm encoded days."
  (apply 'howm-encode-day
         (mapcar 'number-to-string
                 (list (calendar-extract-day date)
                       (calendar-extract-month date)
                       (calendar-extract-year date)))))

(defun calfw-howm--schedule-period (begin end)
  "Return howm schedule items between BEGIN and END."
  (let* ((from (calfw-howm--convert-date begin))
         (to (calfw-howm--convert-date end))
         (filtered (cl-remove-if
                    (lambda (item)
                      (let ((s (howm-schedule-date item)))
                        (or (< s from)
                            (< to s))))
                    (calfw-howm--schedule-get))))
    (howm-schedule-sort-items filtered)))

(defvar calfw-howm-schedule-summary-transformer
  (lambda (line) line)
  "Transformation function to transform howm summary string to calendar title.

If this function splits into a list of string, calfw displays
those string in multi-lines.  The function takes LINE as an
argument.")

(defun calfw-howm--schedule-parse-line (line)
  "Parse LINE and return a list of (date number type summary)."
  (when (string-match "^\\[\\([^@!]+\\)\\]\\([@!]\\)\\([0-9]*\\) \\(.*\\)$" line)
    (list
     (match-string 1 line) (string-to-number (match-string 3 line))
     (match-string 2 line) (match-string 4 line))))

(defun calfw-howm--schedule-period-to-calendar (begin end)
  "Return calfw calendar items between BEGIN and END from howm schedule data."
  (cl-loop with contents = nil
           with periods = nil
           for i in (calfw-howm--schedule-period begin end)
           for date = (calfw-emacs-to-calendar
                       (seconds-to-time (+ 10 (* (howm-schedule-date i) 24 3600))))
           for (datestr num type summary) = (calfw-howm--schedule-parse-line (howm-item-summary i))
           for summary = (funcall calfw-howm-schedule-summary-transformer summary)
           do
           (cond
            ((and (string= type "@") (< 0 num))
             (push (list date (calfw-date-after date (1- num)) summary) periods))
            ((and (string= type "!") (< 0 num))
             (push (list (calfw-date-before date (1- num)) date summary) periods))
            (t
             (setq contents (calfw--contents-add date summary contents))))
           finally return (nconc contents (list (cons 'periods periods)))))

(defun calfw-howm-create-source (&optional color)
  "Create a howm source.

COLOR is the color of the source."
  (make-calfw-source
   :name "howm schedule"
   :color (or color "SteelBlue")
   :update 'calfw-howm-schedule-cache-clear
   :data 'calfw-howm--schedule-period-to-calendar))

(defvar calfw-howm-schedule-map
  (calfw--define-keymap
   '(("RET" . calfw-howm-from-calendar)
     ("q"   . kill-buffer)))
  "Key map for the howm calendar mode.")

(defvar calfw-howm-schedule-contents nil  "A list of calfw-source objects for schedule contents.")
(defvar calfw-howm-annotation-contents nil  "A list of calfw-source objects for annotations.")

(defun calfw-howm-open-calendar ()
  "Open a howm schedule calendar in the new buffer."
  (interactive)
  (save-excursion
    (let ((cp (calfw-create-calendar-component-buffer
               :custom-map calfw-howm-schedule-map
               :view 'month
               :contents-sources (append (list (calfw-howm-create-source))
                                         calfw-howm-schedule-contents)
               :annotation-sources calfw-howm-annotation-contents)))
      (switch-to-buffer (calfw-cp-get-buffer cp)))))

(defun calfw-howm-from-calendar ()
  "Display a howm schedule summary of the date on the cursor.

This command should be executed on the calfw calendar.  Searches
the whole howm data for the date under the cursor."
  (interactive)
  (let* ((mdy (calfw-cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y))))
    (howm-keyword-search key)))

(defun calfw-howm-from-calendar-fast ()
  "Display a howm schedule summary of the date on the cursor from the cache.

This command should be executed on the calfw calendar.
It is faster than `calfw-howm-from-calendar'.
The date is determined by `calfw-cursor-to-nearest-date'."
  (interactive)
  (let* ((mdy (calfw-cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y)))
         (items (calfw-howm--schedule-period mdy mdy)))
    (cond
     ((= 1 (length items))
      (howm-view-open-item (car items)))
     (t
      (howm-view-summary (format "Schedules : %s" (calfw-strtime mdy))
                         items (list key))
      (howm-view-summary-check t)))))

;; (define-key calfw-howm-schedule-map (kbd "RET") 'calfw-howm-from-calendar-fast)
;; (define-key calfw-howm-schedule-inline-keymap (kbd "RET") 'calfw-howm-from-calendar-fast)

;;; Region

(defvar calfw-howm-schedule-inline-keymap
  (calfw--define-keymap
   '(("RET" . calfw-howm-from-calendar)))
  "Key map for the howm inline calendar.")

(defun calfw-howm-schedule-inline (&optional width height view)
  "Create a calendar component region for the howm menu with WIDTH and HEIGHT.

VIEW specifies the initial view."
  (let ((custom-map (copy-keymap calfw-howm-schedule-inline-keymap)) cp)
    (set-keymap-parent custom-map calfw-calendar-mode-map)
    (setq cp (calfw-create-calendar-component-region
              :width width :height (or height 10)
              :keymap custom-map
              :contents-sources (append (list (calfw-howm-create-source))
                                        calfw-howm-schedule-contents)
              :annotation-sources calfw-howm-annotation-contents
              :view (or view 'month))))
  "") ; for null output

;;; Installation

(defun calfw-howm-install-schedules ()
  "Add a schedule collection function to calfw for howm schedule data."
  (interactive)
  (add-hook 'howm-after-save-hook 'calfw-howm-schedule-cache-clear)
  (add-to-list 'howm-menu-allow 'calfw-howm-schedule-inline))

;;; for Elscreen

(when (featurep 'elscreen-howm)
  (defun calfw-howm-elscreen-open-calendar ()
    "Open the calendar in the new screen."
    (interactive)
    (save-current-buffer
      (elscreen-create))
    (calfw-howm-open-calendar))

  (defun calfw-howm-elscreen-kill-calendar ()
    "Kill the calendar buffer and the screen."
    (interactive)
    (kill-buffer nil)
    (unless (elscreen-one-screen-p)
      (elscreen-kill)))

  (define-key calfw-howm-schedule-map (kbd "q")
              'calfw-howm-elscreen-kill-calendar))

(provide 'calfw-howm)
;;; calfw-howm.el ends here
