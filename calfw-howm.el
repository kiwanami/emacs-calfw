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

;; (require 'calfw-howm)
;; (cfw:install-howm-schedules)
;; (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar)

;; If you are using Elscreen, here is useful.
;; (define-key howm-mode-map (kbd "M-C") 'cfw:elscreen-open-howm-calendar)


;;; Code:

(require 'calfw)

(defvar cfw:howm-schedule-cache nil "howmのスケジュールデータのキャッシュ")

(defun cfw:howm-schedule-cache-clear ()
  "howmの全スケジュールデータを返す。キャッシュがあればキャッシュからすぐ返す。"
  (setq cfw:howm-schedule-cache nil))

(defvar cfw:howm-schedule-hook nil "スケジュールのキャッシュを取得し終わった後に呼ばれるフック")

(defun cfw:howm-schedule-get ()
  "howmの全スケジュールデータを返す。キャッシュがあればキャッシュからすぐ返す。"
  (unless cfw:howm-schedule-cache
    (let* ((howm-schedule-types howm-schedule-menu-types)
           (raw (howm-reminder-search howm-schedule-types)))
      (setq cfw:howm-schedule-cache (howm-schedule-sort-items raw)))
    (run-hooks 'cfw:howm-schedule-hook))
  cfw:howm-schedule-cache)

(defun cfw:to-howm-date (date)
  "calendar形式からhowm形式に変換する"
  (apply 'howm-encode-day 
         (mapcar 'number-to-string 
                 (list (calendar-extract-day date)
                       (calendar-extract-month date)
                       (calendar-extract-year date)))))

(defun cfw:howm-schedule-region (begin end)
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
  (lambda (line) (replace-regexp-in-string "^[^@!]+[@!] " "" line))
  "howmのサマリー表示からカレンダー用に変換する関数。リストで返せば複数行で表示する。")

(defun cfw:howm-schedule-region-to-calendar (begin end)
  (loop for i in (cfw:howm-schedule-region begin end)
        for date = (cfw:emacs-to-calendar
                    (seconds-to-time (+ 10 (* (howm-schedule-date i) 24 3600))))
        for line = (funcall cfw:howm-schedule-summary-transformer
                            (howm-item-summary i))
        with contents = nil
        do (cfw:contents-add date line contents)
        finally return contents))

(defvar cfw:howm-schedule-map
  (cfw:define-keymap
   '(
     ("RET" . cfw:howm-from-calendar)
     ("q"   . kill-buffer)
     )))

(defun cfw:open-howm-calendar ()
  (interactive)
  (switch-to-buffer
   (cfw:get-calendar-buffer-custom
    nil nil cfw:howm-schedule-map)))

(defun cfw:howm-from-calendar ()
  (interactive)
  (let* ((mdy (cfw:cursor-to-nearest-date))
         (m (calendar-extract-month mdy))
         (d (calendar-extract-day   mdy))
         (y (calendar-extract-year  mdy))
         (key (format-time-string
               howm-date-format
               (encode-time 0 0 0 d m y))))
    (howm-keyword-search key)))

;;; Region

(defvar cfw:howm-schedule-inline-keymap
  (cfw:define-keymap
   '(("RET" . cfw:howm-from-calendar))))

(defun cfw:howm-schedule-inline (&optional width height)
  (let ((custom-map (copy-keymap cfw:howm-schedule-inline-keymap)))
    (set-keymap-parent custom-map cfw:calendar-mode-map)
    (cfw:insert-calendar-region nil width (or height 10) custom-map)
    ""))

;;; Installation

(defun cfw:install-howm-schedules ()
  (interactive)
  "howmスケジュールからデータを取って表示する。"
  (add-to-list 'cfw:contents-functions 'cfw:howm-schedule-region-to-calendar)
  (add-hook 'howm-after-save-hook 'cfw:howm-schedule-cache-clear)
  (add-to-list 'howm-menu-allow 'cfw:howm-schedule-inline)
  )

;;; for Elscreen

(eval-after-load "elscreen-howm"
  '(progn
     (defun cfw:elscreen-open-howm-calendar ()
       (interactive)
       (save-current-buffer
         (elscreen-create))
       (cfw:open-howm-calendar))

     (defun cfw:elscreen-kill-calendar ()
       (interactive)
       (kill-buffer nil)
       (unless (elscreen-one-screen-p)
         (elscreen-kill)))

     (define-key cfw:howm-schedule-map (kbd "q") 'cfw:elscreen-kill-calendar)
     ))


(provide 'calfw-howm)
;;; calfw-howm.el ends here
