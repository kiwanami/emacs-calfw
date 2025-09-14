;;; calfw-compat.el --- Backward compatibility for calfw -*- lexical-binding: t -*-

;; Copyright (C) 2025 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail dot com>
;; Version: 1.7
;; Keywords: calendar, org
;; Package-Requires: ((emacs "28.1") (calfw "1.7"))
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

;; Defines obsolete aliases so that code that use's calfw old naming scheme
;; does not break, but merely issues warnings.

;;; Code:

(require 'cl-macs)  ;; for cl--find-class

(when nil
  ;; The following is convenience function to generate calls to appropriate
  ;; aliasing functions when the old version of calfw is loaded. The list is
  ;; 95% complete/accurate and required some small modifications.
  (defun calfw-compat--generate-maps ()
    "Generate function call for all symbols starting with `cfw:'.
Output goes into a new buffer, wrapped to `fill-column`."
    (require 'calfw)
    (require 'calfw-org)
    (require 'calfw-ical)
    (require 'calfw-howm)
    (require 'calfw-cal)

    (cl-labels ((new-name (sym)
                  (intern
                   (replace-regexp-in-string
                    "cfw:"
                    "calfw-" (symbol-name sym) nil 'literal))))
      (let* ((syms (apropos-internal "cfw:"))
             (seps '((vars . boundp)
                     (funcs . fboundp)
                     (faces . facep)))
             (all
              (cl-loop for (tag . func) in seps
                       collect
                       (cons tag
                             (cl-loop for sym in (sort (seq-filter func syms)
                                                       'string-lessp)
                                      collect
                                      (cons (new-name sym) sym))))))
        (with-current-buffer (get-buffer-create "*cfw-obsolete*")
          (erase-buffer)
          (emacs-lisp-mode)
          (pp all (current-buffer))
          (pop-to-buffer (current-buffer)))
        all)))
  (calfw-compat--generate-maps))

(defcustom calfw-compat-mark-obsolete "2.0"
  "When non-nil, mark old symbols as obsolete using the value as version."
  :group 'calfw
  :type 'string)

(defvar calfw-compat-aliases
  '((vars
     (calfw-cal-diary-regex . cfw:cal-diary-regex)
     (calfw-cal-schedule-map . cfw:cal-schedule-map)
     (calfw-cal-text-keymap . cfw:cal-text-keymap)
     (calfw-calendar-buffer-name . cfw:calendar-buffer-name)
     (calfw-calendar-mode-hook . cfw:calendar-mode-hook)
     (calfw-calendar-mode-map . cfw:calendar-mode-map)
     (calfw-cp-dipatch-funcs . cfw:cp-dipatch-funcs)
     (calfw-default-text-sorter . cfw:default-text-sorter)
     (calfw-dest-background-buffer . cfw:dest-background-buffer)
     (calfw-details-buffer-name . cfw:details-buffer-name)
     (calfw-details-mode-hook . cfw:details-mode-hook)
     (calfw-details-mode-map . cfw:details-mode-map)
     (calfw-details-window-size . cfw:details-window-size)
     (calfw-display-calendar-holidays . cfw:display-calendar-holidays)
     (calfw-event-format-days-overview . cfw:event-format-days-overview)
     (calfw-event-format-description . cfw:event-format-description)
     (calfw-event-format-detail . cfw:event-format-detail)
     (calfw-event-format-end-date . cfw:event-format-end-date)
     (calfw-event-format-end-time . cfw:event-format-end-time)
     (calfw-event-format-location . cfw:event-format-location)
     (calfw-event-format-overview . cfw:event-format-overview)
     (calfw-event-format-period-overview . cfw:event-format-period-overview)
     (calfw-event-format-start-date . cfw:event-format-start-date)
     (calfw-event-format-start-time . cfw:event-format-start-time)
     (calfw-event-format-title . cfw:event-format-title)
     (calfw-face-item-separator-color . cfw:face-item-separator-color)
     (calfw-fchar-horizontal-line . cfw:fchar-horizontal-line)
     (calfw-fchar-junction . cfw:fchar-junction)
     (calfw-fchar-left-junction . cfw:fchar-left-junction)
     (calfw-fchar-right-junction . cfw:fchar-right-junction)
     (calfw-fchar-top-junction . cfw:fchar-top-junction)
     (calfw-fchar-top-left-corner . cfw:fchar-top-left-corner)
     (calfw-fchar-top-right-corner . cfw:fchar-top-right-corner)
     (calfw-fchar-vertical-line . cfw:fchar-vertical-line)
     (calfw-fstring-period-end . cfw:fstring-period-end)
     (calfw-fstring-period-start . cfw:fstring-period-start)
     (calfw-highlight-today . cfw:highlight-today)
     (calfw-howm-annotation-contents . cfw:howm-annotation-contents)
     (calfw-howm-schedule-cache . cfw:howm-schedule-cache)
     (calfw-howm-schedule-contents . cfw:howm-schedule-contents)
     (calfw-howm-schedule-hook . cfw:howm-schedule-hook)
     (calfw-howm-schedule-inline-keymap . cfw:howm-schedule-inline-keymap)
     (calfw-howm-schedule-map . cfw:howm-schedule-map)
     (calfw-howm-schedule-summary-transformer
      . cfw:howm-schedule-summary-transformer)
     (calfw-ical-calendar-external-shell-command
      . cfw:ical-calendar-external-shell-command)
     (calfw-ical-calendar-tmpbuf . cfw:ical-calendar-tmpbuf)
     (calfw-ical-data-cache . cfw:ical-data-cache)
     (calfw-ical-url-to-buffer-get . cfw:ical-url-to-buffer-get)
     (calfw-org-agenda-schedule-args . cfw:org-agenda-schedule-args)
     (calfw-org-capture-template . cfw:org-capture-template)
     (calfw-org-custom-map . cfw:org-custom-map)
     (calfw-org-face-agenda-item-foreground-color
      . cfw:org-face-agenda-item-foreground-color)
     (calfw-org-icalendars . cfw:org-icalendars)
     (calfw-org-overwrite-default-keybinding
      . cfw:org-overwrite-default-keybinding)
     (calfw-org-schedule-map . cfw:org-schedule-map)
     (calfw-org-schedule-summary-transformer
      . cfw:org-schedule-summary-transformer)
     (calfw-org-text-keymap . cfw:org-text-keymap)
     (calfw-read-date-command . cfw:read-date-command)
     (calfw-render-line-breaker . cfw:render-line-breaker)
     (calfw-week-days . cfw:week-days)
     (calfw-week-friday . cfw:week-friday)
     (calfw-week-monday . cfw:week-monday)
     (calfw-week-saturday . cfw:week-saturday)
     (calfw-week-sunday . cfw:week-sunday)
     (calfw-week-thursday . cfw:week-thursday)
     (calfw-week-tuesday . cfw:week-tuesday)
     (calfw-week-wednesday . cfw:week-wednesday)
     (cl-struct-calfw-component-tags . cl-struct-cfw:component-tags)
     (cl-struct-calfw-dest-tags . cl-struct-cfw:dest-tags)
     (cl-struct-calfw-event-tags . cl-struct-cfw:event-tags)
     (cl-struct-calfw-source-tags . cl-struct-cfw:source-tags))

    (funcs
     (calfw--annotations-merge . cfw:annotations-merge)
     (calfw-cal-create-source . cfw:cal-create-source)
     (calfw-cal--entry-to-event . cfw:cal-entry-to-event)
     (calfw-cal-from-calendar . cfw:cal-from-calendar)
     (calfw-cal-onclick . cfw:cal-onclick)
     (calfw-cal--schedule-period-to-calendar
      . cfw:cal-schedule-period-to-calendar)
     (calfw-calendar-mode . cfw:calendar-mode)
     (calfw-calendar-mode-map . cfw:calendar-mode-map)
     (calfw-calendar-to-emacs . cfw:calendar-to-emacs)
     (calfw-change-view-day . cfw:change-view-day)
     (calfw-change-view-month . cfw:change-view-month)
     (calfw-change-view-two-weeks . cfw:change-view-two-weeks)
     (calfw-change-view-week . cfw:change-view-week)
     (calfw-component-click-hooks . cfw:component-click-hooks)
     (calfw-component-click-hooks--cmacro
      . cfw:component-click-hooks--cmacro)
     (calfw-component-dest . cfw:component-dest)
     (calfw-component-dest--cmacro . cfw:component-dest--cmacro)
     (calfw-component-model . cfw:component-model)
     (calfw-component-model--cmacro
      . cfw:component-model--cmacro)
     (calfw-component-p . cfw:component-p)
     (calfw-component-p--cmacro . cfw:component-p--cmacro)
     (calfw-component-update-hooks . cfw:component-update-hooks)
     (calfw-component-update-hooks--cmacro
      . cfw:component-update-hooks--cmacro)
     (calfw-component-view . cfw:component-view)
     (calfw-component-view--cmacro . cfw:component-view--cmacro)
     (calfw-composite-color . cfw:composite-color)
     (calfw--contents-add . cfw:contents-add)
     (calfw--contents-get . cfw:contents-get)
     (calfw--contents-get-internal . cfw:contents-get-internal)
     (calfw--contents-merge . cfw:contents-merge)
     (calfw--contents-put-source . cfw:contents-put-source)
     (calfw-cp-add-click-hook . cfw:cp-add-click-hook)
     (calfw-cp-add-update-hook . cfw:cp-add-update-hook)
     (calfw--cp-dispatch-view-impl . cfw:cp-dispatch-view-impl)
     (calfw-cp-displayed-date-p . cfw:cp-displayed-date-p)
     (calfw--cp-fire-click-hooks . cfw:cp-fire-click-hooks)
     (calfw--cp-fire-update-hooks . cfw:cp-fire-update-hooks)
     (calfw-cp-get-annotation-sources
      . cfw:cp-get-annotation-sources)
     (calfw-cp-get-buffer . cfw:cp-get-buffer)
     (calfw-cp-get-component . cfw:cp-get-component)
     (calfw-cp-get-contents-sources
      . cfw:cp-get-contents-sources)
     (calfw-cp-get-view . cfw:cp-get-view)
     (calfw-cp-goto-date . cfw:cp-goto-date)
     (calfw--cp-move-cursor . cfw:cp-move-cursor)
     (calfw--cp-new . cfw:cp-new)
     (calfw-cp-resize . cfw:cp-resize)
     (calfw-cp-set-annotation-sources
      . cfw:cp-set-annotation-sources)
     (calfw-cp-set-contents-sources
      . cfw:cp-set-contents-sources)
     (calfw-cp-set-view . cfw:cp-set-view)
     (calfw--cp-update . cfw:cp-update)
     (calfw-create-calendar-component-buffer
      . cfw:create-calendar-component-buffer)
     (calfw-create-calendar-component-region
      . cfw:create-calendar-component-region)
     (calfw--cursor-to-date . cfw:cursor-to-date)
     (calfw-cursor-to-nearest-date . cfw:cursor-to-nearest-date)
     (calfw-date . cfw:date)
     (calfw-date-after . cfw:date-after)
     (calfw-date-before . cfw:date-before)
     (calfw-date-between . cfw:date-between)
     (calfw-date-less-equal-p . cfw:date-less-equal-p)
     (calfw-days-diff . cfw:days-diff)
     (calfw-ical-decode-to-calendar . cfw:decode-to-calendar)
     (calfw--define-keymap . cfw:define-keymap)
     (calfw-dest-after-update . cfw:dest-after-update)
     (calfw-dest-after-update-func . cfw:dest-after-update-func)
     (calfw-dest-after-update-func--cmacro
      . cfw:dest-after-update-func--cmacro)
     (calfw-dest-before-update . cfw:dest-before-update)
     (calfw-dest-before-update-func
      . cfw:dest-before-update-func)
     (calfw-dest-before-update-func--cmacro
      . cfw:dest-before-update-func--cmacro)
     (calfw-dest-buffer . cfw:dest-buffer)
     (calfw-dest-buffer--cmacro . cfw:dest-buffer--cmacro)
     (calfw-dest-clear . cfw:dest-clear)
     (calfw-dest-clear-func . cfw:dest-clear-func)
     (calfw-dest-clear-func--cmacro
      . cfw:dest-clear-func--cmacro)
     (calfw-dest-height . cfw:dest-height)
     (calfw-dest-height--cmacro . cfw:dest-height--cmacro)
     (calfw-dest-init-buffer . cfw:dest-init-buffer)
     (calfw-dest-init-inline . cfw:dest-init-inline)
     (calfw-dest-init-region . cfw:dest-init-region)
     (calfw-dest-max-func . cfw:dest-max-func)
     (calfw-dest-max-func--cmacro . cfw:dest-max-func--cmacro)
     (calfw-dest-min-func . cfw:dest-min-func)
     (calfw-dest-min-func--cmacro . cfw:dest-min-func--cmacro)
     (calfw--dest-ol-today-clear . cfw:dest-ol-today-clear)
     (calfw--dest-ol-today-set . cfw:dest-ol-today-set)
     (calfw-dest-p . cfw:dest-p)
     (calfw-dest-p--cmacro . cfw:dest-p--cmacro)
     (calfw-dest-point-max . cfw:dest-point-max)
     (calfw-dest-point-min . cfw:dest-point-min)
     (calfw--dest-region-clear . cfw:dest-region-clear)
     (calfw-dest-today-ol . cfw:dest-today-ol)
     (calfw-dest-today-ol--cmacro . cfw:dest-today-ol--cmacro)
     (calfw-dest-type . cfw:dest-type)
     (calfw-dest-type--cmacro . cfw:dest-type--cmacro)
     (calfw-dest-width . cfw:dest-width)
     (calfw-dest-width--cmacro . cfw:dest-width--cmacro)
     (calfw-dest-with-region . cfw:dest-with-region)
     (calfw--details-find-item . cfw:details-find-item)
     (calfw-details-kill-buffer-command
      . cfw:details-kill-buffer-command)
     (calfw-details-layout . cfw:details-layout)
     (calfw-details-mode . cfw:details-mode)
     (calfw-details-navi-next-command
      . cfw:details-navi-next-command)
     (calfw-details-navi-next-item-command
      . cfw:details-navi-next-item-command)
     (calfw-details-navi-prev-command
      . cfw:details-navi-prev-command)
     (calfw-details-navi-prev-item-command
      . cfw:details-navi-prev-item-command)
     (calfw-details-popup . cfw:details-popup)
     (calfw-emacs-to-calendar . cfw:emacs-to-calendar)
     (calfw-enumerate-days . cfw:enumerate-days)
     (calfw-event-data . cfw:event-data)
     (calfw-event-data--cmacro . cfw:event-data--cmacro)
     (calfw-event-days-overview . cfw:event-days-overview)
     (calfw-event-description . cfw:event-description)
     (calfw-event-description--cmacro
      . cfw:event-description--cmacro)
     (calfw-event-detail . cfw:event-detail)
     (calfw-event-end-date . cfw:event-end-date)
     (calfw-event-end-date--cmacro . cfw:event-end-date--cmacro)
     (calfw-event-end-time . cfw:event-end-time)
     (calfw-event-end-time--cmacro . cfw:event-end-time--cmacro)
     (calfw-event-format . cfw:event-format)
     (calfw--event-format-field . cfw:event-format-field)
     (calfw--event-format-field-date
      . cfw:event-format-field-date)
     (calfw--event-format-field-number
      . cfw:event-format-field-number)
     (calfw--event-format-field-string
      . cfw:event-format-field-string)
     (calfw--event-format-field-time
      . cfw:event-format-field-time)
     (calfw-event-location . cfw:event-location)
     (calfw-event-location--cmacro . cfw:event-location--cmacro)
     (calfw-event-mouse-click-toggle-calendar
      . cfw:event-mouse-click-toggle-calendar)
     (calfw-event-overview . cfw:event-overview)
     (calfw-event-p . cfw:event-p)
     (calfw-event-p--cmacro . cfw:event-p--cmacro)
     (calfw-event-period-overview . cfw:event-period-overview)
     (calfw-event-source . cfw:event-source)
     (calfw-event-source--cmacro . cfw:event-source--cmacro)
     (calfw-event-start-date . cfw:event-start-date)
     (calfw-event-start-date--cmacro
      . cfw:event-start-date--cmacro)
     (calfw-event-start-time . cfw:event-start-time)
     (calfw-event-start-time--cmacro
      . cfw:event-start-time--cmacro)
     (calfw-event-status . cfw:event-status)
     (calfw-event-status--cmacro . cfw:event-status--cmacro)
     (calfw-event-title . cfw:event-title)
     (calfw-event-title--cmacro . cfw:event-title--cmacro)
     (calfw-event-toggle-all-calendars
      . cfw:event-toggle-all-calendars)
     (calfw-event-toggle-calendar . cfw:event-toggle-calendar)
     (calfw--extract-text-props . cfw:extract-text-props)
     (calfw--fill-keymap-property . cfw:fill-keymap-property)
     (calfw--find-all-by-date . cfw:find-all-by-date)
     (calfw--find-by-date . cfw:find-by-date)
     (calfw--find-first-date . cfw:find-first-date)
     (calfw--find-item . cfw:find-item)
     (calfw--find-last-date . cfw:find-last-date)
     (calfw-flatten . cfw:flatten)
     (calfw-get-calendar-text . cfw:get-calendar-text)
     (calfw-howm-create-source . cfw:howm-create-source)
     (calfw-howm-from-calendar . cfw:howm-from-calendar)
     (calfw-howm-from-calendar-fast
      . cfw:howm-from-calendar-fast)
     (calfw-howm-schedule-cache-clear
      . cfw:howm-schedule-cache-clear)
     (calfw-howm--schedule-get . cfw:howm-schedule-get)
     (calfw-howm-schedule-inline . cfw:howm-schedule-inline)
     (calfw-howm--schedule-parse-line
      . cfw:howm-schedule-parse-line)
     (calfw-howm--schedule-period . cfw:howm-schedule-period)
     (calfw-howm--schedule-period-to-calendar
      . cfw:howm-schedule-period-to-calendar)
     (calfw-ical-convert-event . cfw:ical-convert-event)
     (calfw-ical-convert-ical-to-calfw
      . cfw:ical-convert-ical-to-calfw)
     (calfw-ical-create-source . cfw:ical-create-source)
     (calfw-ical-data-cache-clear . cfw:ical-data-cache-clear)
     (calfw-ical-data-cache-clear-all
      . cfw:ical-data-cache-clear-all)
     (calfw-ical-debug . cfw:ical-debug)
     (calfw-ical-event-get-dates . cfw:ical-event-get-dates)
     (calfw-ical-get-data . cfw:ical-get-data)
     (calfw-ical-normalize-buffer . cfw:ical-normalize-buffer)
     (calfw-ical-sanitize-string . cfw:ical-sanitize-string)
     (calfw-ical-to-calendar . cfw:ical-to-calendar)
     (calfw-ical-url-to-buffer . cfw:ical-url-to-buffer)
     (calfw-ical-url-to-buffer-external . cfw:ical-url-to-buffer-external)
     (calfw-ical-url-to-buffer-internal . cfw:ical-url-to-buffer-internal)
     (calfw-ical-with-buffer . cfw:ical-with-buffer)
     (calfw-howm-install-schedules . cfw:install-howm-schedules)
     (calfw--k . cfw:k)
     (calfw-make-bg-color . cfw:make-bg-color)
     (calfw-make-fg-color . cfw:make-fg-color)
     (calfw-model-abstract-derived . cfw:model-abstract-derived)
     (calfw-model-abstract-new . cfw:model-abstract-new)
     (calfw--model-create-updated-view-data
      . cfw:model-create-updated-view-data)
     (calfw-model-get-annotation-by-date
      . cfw:model-get-annotation-by-date)
     (calfw--model-get-annotation-sources
      . cfw:model-get-annotation-sources)
     (calfw-model-get-contents-by-date
      . cfw:model-get-contents-by-date)
     (calfw--model-get-contents-sources
      . cfw:model-get-contents-sources)
     (calfw-model-get-holiday-by-date
      . cfw:model-get-holiday-by-date)
     (calfw-model-get-periods-by-date
      . cfw:model-get-periods-by-date)
     (calfw-model-get-sorter . cfw:model-get-sorter)
     (calfw--model-set-annotation-sources
      . cfw:model-set-annotation-sources)
     (calfw--model-set-contents-sources . cfw:model-set-contents-sources)
     (calfw--model-set-init-date . cfw:model-set-init-date)
     (calfw-month-year-contain-p . cfw:month-year-contain-p)
     (calfw-month-year-equal-p . cfw:month-year-equal-p)
     (calfw-navi-goto-date . cfw:navi-goto-date)
     (calfw-navi-goto-date-command . cfw:navi-goto-date-command)
     (calfw-navi-goto-first-date-command
      . cfw:navi-goto-first-date-command)
     (calfw-navi-goto-last-date-command . cfw:navi-goto-last-date-command)
     (calfw-navi-goto-today-command . cfw:navi-goto-today-command)
     (calfw-navi-goto-week-begin-command
      . cfw:navi-goto-week-begin-command)
     (calfw-navi-goto-week-end-command . cfw:navi-goto-week-end-command)
     (calfw-navi-next-day-command . cfw:navi-next-day-command)
     (calfw-navi-next-item-command . cfw:navi-next-item-command)
     (calfw-navi-next-month-command . cfw:navi-next-month-command)
     (calfw-navi-next-week-command . cfw:navi-next-week-command)
     (calfw-navi-on-click . cfw:navi-on-click)
     (calfw-navi-prev-item-command . cfw:navi-prev-item-command)
     (calfw-navi-previous-day-command . cfw:navi-previous-day-command)
     (calfw-navi-previous-month-command . cfw:navi-previous-month-command)
     (calfw-navi-previous-week-command . cfw:navi-previous-week-command)
     (calfw-open-calendar-buffer . cfw:open-calendar-buffer)
     (calfw-open-debug-calendar . cfw:open-debug-calendar)
     (calfw-cal-open-diary-calendar . cfw:open-diary-calendar)
     (calfw-howm-open-calendar . cfw:open-howm-calendar)
     (calfw-ical-open-calendar . cfw:open-ical-calendar)
     (calfw-org-open-calendar . cfw:open-org-calendar)
     (calfw-org-capture . cfw:org-capture)
     (calfw-org-capture-day . cfw:org-capture-day)
     (calfw-org-clean-exit . cfw:org-clean-exit)
     (calfw-org--collect-schedules-period
      . cfw:org-collect-schedules-period)
     (calfw-org-convert-event . cfw:org-convert-event)
     (calfw-org-convert-org-to-calfw . cfw:org-convert-org-to-calfw)
     (calfw-org-create-file-source . cfw:org-create-file-source)
     (calfw-org-create-source . cfw:org-create-source)
     (calfw-org--extract-summary . cfw:org-extract-summary)
     (calfw-org-filter-datetime . cfw:org-filter-datetime)
     (calfw-org-format-date . cfw:org-format-date)
     (calfw-org-format-title . cfw:org-format-title)
     (calfw-org-get-timerange . cfw:org-get-timerange)
     (calfw-org-goto-date . cfw:org-goto-date)
     (calfw-org-jump-map . cfw:org-jump-map)
     (calfw-org-normalize-date . cfw:org-normalize-date)
     (calfw-org-onclick . cfw:org-onclick)
     (calfw-org-open-agenda-day . cfw:org-open-agenda-day)
     (calfw-org-read-date-command . cfw:org-read-date-command)
     (calfw-org--schedule-period-to-calendar
      . cfw:org-schedule-period-to-calendar)
     (calfw-org-schedule-sorter . cfw:org-schedule-sorter)
     (calfw-org-schedule-sorter2 . cfw:org-schedule-sorter2)
     (calfw-org-summary-format . cfw:org-summary-format)
     (calfw-org-to-calendar . cfw:org-to-calendar)
     (calfw-org--tp . cfw:org-tp)
     (calfw-parse-str-time . cfw:parse-str-time)
     (calfw-parsetime . cfw:parsetime)
     (calfw-parsetime-emacs . cfw:parsetime-emacs)
     (calfw-periods-put-source . cfw:periods-put-source)
     (calfw-read-date-command-simple . cfw:read-date-command-simple)
     (calfw-refresh-calendar-buffer . cfw:refresh-calendar-buffer)
     (calfw--render-add-item-separator-sign
      . cfw:render-add-item-separator-sign)
     (calfw--render-add-right . cfw:render-add-right)
     (calfw--render-append-parts . cfw:render-append-parts)
     (calfw--render-break-lines . cfw:render-break-lines)
     (calfw--render-button . cfw:render-button)
     (calfw--render-calendar-cells-days . cfw:render-calendar-cells-days)
     (calfw--render-calendar-cells-weeks . cfw:render-calendar-cells-weeks)
     (calfw--render-center . cfw:render-center)
     (calfw--render-columns . cfw:render-columns)
     (calfw--render-day-of-week-names
      . cfw:render-day-of-week-names)
     (calfw--render-default-content-face
      . cfw:render-default-content-face)
     (calfw--render-event-days-overview-content
      . cfw:render-event-days-overview-content)
     (calfw--render-event-details-content
      . cfw:render-event-details-content)
     (calfw--render-event-overview-content
      . cfw:render-event-overview-content)
     (calfw--render-footer . cfw:render-footer)
     (calfw--render-get-face-content
      . cfw:render-get-face-content)
     (calfw--render-get-face-period . cfw:render-get-face-period)
     (calfw--render-get-week-face . cfw:render-get-week-face)
     (calfw--render-left . cfw:render-left)
     (calfw-render-line-breaker-none
      . cfw:render-line-breaker-none)
     (calfw-render-line-breaker-simple
      . cfw:render-line-breaker-simple)
     (calfw-render-line-breaker-wordwrap
      . cfw:render-line-breaker-wordwrap)
     (calfw--render-map-event-content
      . cfw:render-map-event-content)
     (calfw--render-periods . cfw:render-periods)
     (calfw--render-periods-days . cfw:render-periods-days)
     (calfw--render-periods-get-min
      . cfw:render-periods-get-min)
     (calfw--render-periods-place . cfw:render-periods-place)
     (calfw--render-periods-stacks . cfw:render-periods-stacks)
     (calfw--render-periods-title . cfw:render-periods-title)
     (calfw--render-right . cfw:render-right)
     (calfw--render-rows-prop . cfw:render-rows-prop)
     (calfw--render-separator . cfw:render-separator)
     (calfw--render-sort-contents . cfw:render-sort-contents)
     (calfw-render-title-day . cfw:render-title-day)
     (calfw-render-title-month . cfw:render-title-month)
     (calfw-render-title-period . cfw:render-title-period)
     (calfw--render-toolbar . cfw:render-toolbar)
     (calfw--render-truncate . cfw:render-truncate)
     (calfw--round-cell-width . cfw:round-cell-width)
     (calfw--rt . cfw:rt)
     (calfw-show-details-command . cfw:show-details-command)
     (calfw-source-color . cfw:source-color)
     (calfw-source-color--cmacro . cfw:source-color--cmacro)
     (calfw-source-data . cfw:source-data)
     (calfw-source-data--cmacro . cfw:source-data--cmacro)
     (calfw-source-hidden . cfw:source-hidden)
     (calfw-source-hidden--cmacro . cfw:source-hidden--cmacro)
     (calfw-source-name . cfw:source-name)
     (calfw-source-name--cmacro . cfw:source-name--cmacro)
     (calfw-source-opt-face . cfw:source-opt-face)
     (calfw-source-opt-face--cmacro
      . cfw:source-opt-face--cmacro)
     (calfw-source-opt-period-face
      . cfw:source-opt-period-face)
     (calfw-source-opt-period-face--cmacro
      . cfw:source-opt-period-face--cmacro)
     (calfw-source-p . cfw:source-p)
     (calfw-source-p--cmacro . cfw:source-p--cmacro)
     (calfw-source-period-bgcolor . cfw:source-period-bgcolor)
     (calfw-source-period-bgcolor--cmacro
      . cfw:source-period-bgcolor--cmacro)
     (calfw--source-period-bgcolor-get
      . cfw:source-period-bgcolor-get)
     (calfw-source-period-fgcolor . cfw:source-period-fgcolor)
     (calfw-source-period-fgcolor--cmacro
      . cfw:source-period-fgcolor--cmacro)
     (calfw--source-period-fgcolor-get
      . cfw:source-period-fgcolor-get)
     (calfw-source-update . cfw:source-update)
     (calfw-source-update--cmacro . cfw:source-update--cmacro)
     (calfw-strtime . cfw:strtime)
     (calfw-strtime-emacs . cfw:strtime-emacs)
     (calfw--sym . cfw:sym)
     (calfw-time . cfw:time)
     (calfw-howm--convert-date . cfw:to-howm-date)
     (calfw--tp . cfw:tp)
     (calfw--view-day . cfw:view-day)
     (calfw--view-day-calc-param . cfw:view-day-calc-param)
     (calfw--view-model-make-common-data
      . cfw:view-model-make-common-data)
     (calfw--view-model-make-common-data-for-days
      . cfw:view-model-make-common-data-for-days)
     (calfw--view-model-make-common-data-for-weeks
      . cfw:view-model-make-common-data-for-weeks)
     (calfw--view-model-make-day-names-for-days
      . cfw:view-model-make-day-names-for-days)
     (calfw--view-model-make-day-names-for-week
      . cfw:view-model-make-day-names-for-week)
     (calfw--view-model-make-days . cfw:view-model-make-days)
     (calfw--view-model-make-holidays . cfw:view-model-make-holidays)
     (calfw--view-model-make-weeks . cfw:view-model-make-weeks)
     (calfw--view-month . cfw:view-month)
     (calfw--view-month-calc-param . cfw:view-month-calc-param)
     (calfw--view-month-model . cfw:view-month-model)
     (calfw--view-two-weeks . cfw:view-two-weeks)
     (calfw--view-two-weeks-calc-param . cfw:view-two-weeks-calc-param)
     (calfw--view-two-weeks-model . cfw:view-two-weeks-model)
     (calfw-view-two-weeks-model-adjust . cfw:view-two-weeks-model-adjust)
     (calfw--view-week . cfw:view-week)
     (calfw--view-week-calc-param . cfw:view-week-calc-param)
     (calfw--view-week-model . cfw:view-week-model)
     (calfw-week-begin-date . cfw:week-begin-date)
     (calfw-week-end-date . cfw:week-end-date)
     (calfw-howm-elscreen-open-calendar . cfw:elscreen-open-howm-calendar)
     (calfw-howm-elscreen-kill-calendar . cfw:elscreen-kill-calendar)
     (copy-calfw-component . copy-cfw:component)
     (copy-calfw-dest . copy-cfw:dest)
     (copy-calfw-event . copy-cfw:event)
     (copy-calfw-source . copy-cfw:source)
     (make-calfw-component . make-cfw:component)
     (make-calfw-component--cmacro . make-cfw:component--cmacro)
     (make-calfw-dest . make-cfw:dest)
     (make-calfw-dest--cmacro . make-cfw:dest--cmacro)
     (make-calfw-event . make-cfw:event)
     (make-calfw-event--cmacro . make-cfw:event--cmacro)
     (make-calfw-source . make-cfw:source)
     (make-calfw-source--cmacro . make-cfw:source--cmacro))

    (faces
     (calfw-face-annotation . cfw:face-annotation)
     (calfw-face-calendar-hidden . cfw:face-calendar-hidden)
     (calfw-face-day-title . cfw:face-day-title)
     (calfw-face-default-content . cfw:face-default-content)
     (calfw-face-default-day . cfw:face-default-day)
     (calfw-face-disable . cfw:face-disable)
     (calfw-face-grid . cfw:face-grid)
     (calfw-face-header . cfw:face-header)
     (calfw-face-holiday . cfw:face-holiday)
     (calfw-face-periods . cfw:face-periods)
     (calfw-face-saturday . cfw:face-saturday)
     (calfw-face-sunday . cfw:face-sunday)
     (calfw-face-title . cfw:face-title)
     (calfw-face-today . cfw:face-today)
     (calfw-face-today-title . cfw:face-today-title)
     (calfw-face-toolbar . cfw:face-toolbar)
     (calfw-face-toolbar-button-off . cfw:face-toolbar-button-off)
     (calfw-face-toolbar-button-on . cfw:face-toolbar-button-on))))


(let ((alias  (if calfw-compat-mark-obsolete
                  (lambda (old new)
                    (define-obsolete-variable-alias
                      old new calfw-compat-mark-obsolete))
                #'defvaralias)))
  (dolist (pair (alist-get 'vars calfw-compat-aliases))
    (funcall alias (cdr pair) (car pair))))

(let ((alias (if calfw-compat-mark-obsolete
                 (lambda (old new)
                   (define-obsolete-function-alias
                     old new calfw-compat-mark-obsolete))
               #'defalias)))
  (dolist (pair (alist-get 'funcs calfw-compat-aliases))
    (funcall alias (cdr pair) (car pair))))

(let ((alias (if calfw-compat-mark-obsolete
                 (lambda (old new)
                   (define-obsolete-face-alias
                    old new calfw-compat-mark-obsolete))
               (lambda (old new) (put old 'face-alias new)))))
  (dolist (pair (alist-get 'faces calfw-compat-aliases))
    (funcall alias (cdr pair) (car pair))))

;; a HACK to alias old names of structures
(put 'cfw:event 'cl--class (cl--find-class 'calfw-event))
(put 'cfw:source 'cl--class (cl--find-class 'calfw-source))
(put 'cfw:componenet 'cl--class (cl--find-class 'calfw-component))
(put 'cfw:event 'cl--class (cl--find-class 'calfw-event))
(put 'cfw:dest 'cl--class (cl--find-class 'calfw-dest))

(defun calfw-compat-update-symbols (files)
  "Replace old symbols with new ones in FILES, based on `calfw-compat-aliases'."
  (dolist (f files)
    (when (file-exists-p f)
      (with-current-buffer (find-file-noselect f)
        (save-excursion
          (dolist (lst calfw-compat-aliases)
            (dolist (pair (cdr lst))
              (goto-char (point-min))
              (let ((regexp (format "\\_<%s\\_>"
                                    (regexp-quote (symbol-name (cdr pair)))))
                    (new (symbol-name (car pair))))
                (while (re-search-forward regexp nil t)
                  (replace-match new t t))))))
        (when (buffer-modified-p)
          (save-buffer))))))

;; (calfw-compat-update-symbolsd (directory-files default-directory t ".*\\.el"))

(provide 'calfw-compat)
;;; calfw-compat.el ends here
