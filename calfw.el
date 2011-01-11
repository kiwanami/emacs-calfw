;;; calfw.el --- Calendar view framework on Emacs

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

;; This program is a framework for the Calendar component. In the
;; Emacs, uses can show schedules in the calendar views, like iCal,
;; Outlook and Google Calendar.

;;; Installation:

;; Place this program in your load path and add following code.

;; (require 'calfw)

;;; Usage:

;; Executing the command `cfw:open-calendar-buffer', switch to the calendar buffer.
;; You can navigate the date like calendar.el.

;; Schedule data which are shown in the calendar view, are collected
;; by the variables `cfw:contents-functions' and
;; `cfw:annotations-functions'. The former variable defines schedule
;; contents. The later one does date annotations like the moon phases.
;; This program gets the holidays using the function
;; `calendar-holiday-list'. See the document for the holidays.el and
;; the Info text.

;;; Add-ons:

;; Following programs are also useful:
;; - calfw-howm.el : Display howm schedules
;; - calfw-ical.el : Display schedules of the iCalendar format.

;;; Code:

(eval-when-compile (require 'cl))
(require 'calendar)
(require 'holidays)



;;; Constants

(defconst cfw:week-sunday    0)
(defconst cfw:week-monday    1)
(defconst cfw:week-tuesday   2)
(defconst cfw:week-wednesday 3)
(defconst cfw:week-thursday  4)
(defconst cfw:week-friday    5)
(defconst cfw:week-saturday  6)
(defconst cfw:week-days      7)

;;; Faces

(defface cfw:face-title
  '((((class color) (background light))
     :foreground "DarkGrey" :weight bold :height 2.0 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "yellow" :weight bold :height 2.0 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for title" :group 'calfw)

(defface cfw:face-header
  '((((class color)) :foreground "Slategray4" :background "Gray90" :weight bold))
  "Face for headers" :group 'calfw)

(defface cfw:face-sunday
  '((((class color) (background light))
     :foreground "red2" :background "#ffd5e5" :weight bold)
    (((class color) (background dark))
     :foreground "red" :background "chocolate4" :weight bold))
  "Face for Sunday" :group 'calfw)

(defface cfw:face-saturday
  '((((class color) (background light))
     :foreground "Blue" :background "#d4e5ff" :weight bold)
    (((class color) (background light))
     :foreground "Darkslategray3" :background "Steelblue4" :weight bold))
  "Face for Saturday" :group 'calfw)

(defface cfw:face-holiday
  '((((class color) (background light))
     :background "#ffd5e5")
    (((class color) (background dark))
     :background "chocolate4"))
  "Face for holidays" :group 'calfw)

(defface cfw:face-grid
  '((((class color) (background light))
     :foreground "SlateBlue")
    (((class color) (background dark))
     :foreground "DarkGrey"))
  "Face for grids"
  :group 'calfw)

(defface cfw:face-default-content
  '((((class color)) :foreground "#2952a3"))
  "Face for default contents"
  :group 'calfw)

(defface cfw:face-periods
  '((((class color) (background light))
     :background "#668cd9" :foreground "White" :slant italic)
    (((class color) (background dark))
     :background "#d4e5ff" :foreground "RoyalBlue" :slant italic))
  "Face for period" :group 'calfw)

(defface cfw:face-day-title
  '((((class color) (background light))
     :background "#f8f9ff")
    (((class color) (background dark))
     :background "DarkGrey"))
  "Face for day title"
  :group 'calfw)

(defface cfw:face-default-day
  '((((class color) (background light))
     :weight bold :inherit cfw:face-day-title)
    (((class color) (background dark))
     :weight bold :inherit cfw:face-day-title))
  "Face for default day" :group 'calfw)

(defface cfw:face-annotation
  '((((class color)) :foreground "RosyBrown" :inherit cfw:face-day-title))
  "Face for annotations"
  :group 'calfw)

(defface cfw:face-today-title
  '((((class color) (background light))
     :background "#fad163")
    (((class color) (background dark))
     :background "DarkCyan"))
  "Face for today" :group 'calfw)

(defface cfw:face-today
  '((((class color) (background light))
     :background "#fff7d7")
    (((class color) (background dark))
     :background "Cyan"))
  "Face for today" :group 'calfw)

(defface cfw:face-select
  '((((class color) (background light))
     :background "#c3c9f8")
    (((class color) (background dark))
     :background "Blue"))
  "Face for selection" :group 'calfw)



;;; Utilities

(defun cfw:k (key alist)
  "[internal] Get a content by key from the given alist."
  (cdr (assq key alist)))

(defun cfw:rt (text face)
  "[internal] Put a face to the given text."
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) 'face face text)
  (put-text-property 0 (length text) 'font-lock-face face text)
  text)

(defun cfw:tp (text prop value)
  "[internal] Put a text property to the entire text string."
  (if (< 0 (length text))
    (put-text-property 0 (length text) prop value text))
  text)

(defun cfw:define-keymap (keymap-list)
  "[internal] Key map definition utility. 
KEYMAP-LIST is a source list like ((key . command) ... )."
  (let ((map (make-sparse-keymap)))
    (mapc 
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro (car i)) (car i))
         (cdr i)))
     keymap-list)
    map))

(defun cfw:trim (str)
  "[internal] Trim the space char-actors."
  (if (string-match "^[ \t\n\r]*\\(.*?\\)[ \t\n\r]*$" str)
      (match-string 1 str)
    str))

(defun cfw:copy-list (list)
  "[internal] [imported from cl.el] Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure
itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))



;;; Date Time Transformation

(defun cfw:date (month day year)
  "Construct a date object in the calendar format."
  (and month day year
       (list month day year)))

(defun cfw:emacs-to-calendar (time)
  "Transform an emacs time format to a calendar one."
  (let ((dt (decode-time time)))
    (list (nth 4 dt) (nth 3 dt) (nth 5 dt))))

(defun cfw:calendar-to-emacs (date)
  "Transform a calendar time format to an emacs one."
  (encode-time 0 0 0
               (calendar-extract-day date) 
               (calendar-extract-month date)
               (calendar-extract-year date)))

(defun cfw:month-year-equal-p (date1 date2)
  "Return `t' if numbers of month and year of DATE1 is equals to
ones of DATE2. Otherwise is `nil'."
  (and 
   (= (calendar-extract-month date1)
      (calendar-extract-month date2))
   (= (calendar-extract-year date1)
      (calendar-extract-year date2))))

(defun cfw:date-less-equal-p (d1 d2)
  "Return `t' if date value D1 is less than or equals to date value D2."
  (let ((ed1 (cfw:calendar-to-emacs d1))
        (ed2 (cfw:calendar-to-emacs d2)))
    (or (equal ed1 ed2)
        (time-less-p ed1 ed2))))

(defun cfw:date-between (begin end date)
  "Return `t' if date value DATE exists between BEGIN and END."
  (and (cfw:date-less-equal-p begin date)
       (cfw:date-less-equal-p date end)))

(defun cfw:month-year-contain-p (month year date2)
  "Return `t' if date value DATE2 is included in MONTH and YEAR."
  (and 
   (= month (calendar-extract-month date2))
   (= year (calendar-extract-year date2))))

(defun cfw:strtime-emacs (time)
  "Format emacs time value TIME to the string form YYYY/MM/DD."
  (format-time-string "%Y/%m/%d" time))

(defun cfw:strtime (date)
  "Format calendar date value DATE to the string form YYYY/MM/DD."
  (cfw:strtime-emacs (cfw:calendar-to-emacs date)))

(defun cfw:parsetime-emacs (str)
  "Transform the string format YYYY/MM/DD to an emacs time value."
  (when (string-match "\\([0-9]+\\)\\/\\([0-9]+\\)\\/\\([0-9]+\\)" str)
     (apply 'encode-time 
            (let (ret)
              (dotimes (i 6)
                (push (string-to-number (or (match-string (+ i 1) str) "0")) ret))
              ret))))

(defun cfw:parsetime (str)
  "Transform the string format YYYY/MM/DD to a calendar date value."
  (cfw:emacs-to-calendar (cfw:parsetime-emacs str)))

(defun cfw:enumerate-days (begin end)
  "Enumerate date objects between BEGIN and END."
  (when (> (calendar-absolute-from-gregorian begin)
           (calendar-absolute-from-gregorian end))
    (error "Invalid period : %S - %S" begin end))
  (let ((d begin) ret (cont t))
    (while cont
      (push (cfw:copy-list d) ret)
      (setq cont (not (equal d end)))
      (setq d (calendar-gregorian-from-absolute
               (1+ (calendar-absolute-from-gregorian d)))))
    (nreverse ret)))


;;; Rendering destination

;; This structure object is the abstraction of the rendering
;; destinations, such as buffers, regions and so on.

;; [cfw:dest]
;; type        : identify symbol for destination type. (buffer, region, text)
;; model       : a model object to be displayed
;; buffer      : a buffer object of rendering destination.
;; min-func    : a function that returns upper limit of rendering destination.
;; max-func    : a function that returns lower limit of rendering destination.
;; width       : width of the reference size.
;; height      : height of the reference size.
;; clear-func  : a function that clears the rendering destination.
;; update-func : a function that is called at the end of rendering routine.

(defstruct cfw:dest type model buffer min-func max-func width height clear-func update-func)

;; shortcut functions

(defmacro cfw:dest-with-region (dest &rest body)
  `(save-restriction
     (narrow-to-region 
      (cfw:dest-point-min dest) (cfw:dest-point-max dest))
     ,@body))
(put 'cfw:dest-with-region 'lisp-indent-function 1)

(defun cfw:dest-point-min (c)
  (funcall (cfw:dest-min-func c)))

(defun cfw:dest-point-max (c)
  (funcall (cfw:dest-max-func c)))

(defun cfw:dest-clear (c)
  (funcall (cfw:dest-clear-func c)))

(defun cfw:dest-update (c)
  (when (cfw:dest-update-func c)
    (funcall (cfw:dest-update-func c))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

;; * Contents functions

;; A list of contents functions. A "Content" means a schedule on a
;; day. The "contents function" receives two arguments, begin date and
;; end date, and returns a list of cons cells of (DATE . ("ContentA"
;; "ContentB" ...)), that is, alist of DATE and a list of texts
;; between the begin date and end one. The alist should not be
;; ordered, but the dates should be unique. The content texts of the
;; schedules can have text properties and the properties are displayed
;; in the view.

;; The contents function can return periods of schedules. Then, the
;; alist has the record that consists of symbol `periods' and a list
;; of period lists. A period list has begin date, end date and content
;; text.

;; Input  : begin[DATE] end[DATE]
;; Output : '((DATE CONTENT1 CONTENT2 ...) (DATE CONTENT ... )
;;            (periods (DATE DATE CONTENT) (DATE DATE CONTENT) ... ))

(defvar cfw:contents-functions nil "A list of contents functions.")

;; * Annotations functions

;; A list of annotation functions. The "annotation function" receives
;; two arguments, begin date and end date, and returns an alist of
;; DATE and annotation text. If more than two functions return
;; annotations on the same day, those annotations are joined into one
;; text.

(defvar cfw:annotations-functions nil "A list of annotation functions")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level API

;; buffer

(defun cfw:open-calendar-buffer (&optional date)
  "Open a calendar buffer simply.
DATE is initial focus date. If it is nil, today is selected
initially.  This function uses the function
`cfw:get-calendar-buffer-custom' internally."
  (interactive)
  (switch-to-buffer (cfw:get-calendar-buffer-custom date)))

(defun cfw:get-calendar-buffer-custom (&optional date buffer custom-map)
  "Return a calendar buffer with some customize parameters.

This function binds the rendering destination object at the
buffer local variable `cfw:dest'.

The size of calendar is calculated from the window that shows
BUFFER or the selected window.  DATE is initial focus date. If it
is nil, today is selected initially.  BUFFER is the buffer to be
rendered. If BUFFER is nil, this function creates a new buffer
named `cfw:calendar-buffer-name'.  CUSTOM-MAP is the additional
keymap that is added to default keymap `cfw:calendar-mode-map'."
  (let* ((dest (cfw:dest-init-buffer buffer nil nil custom-map))
         (buf (cfw:dest-buffer dest)))
    (cfw:calendar-update dest)
    (with-current-buffer buf
      (cfw:navi-goto-date (or date (calendar-current-date))))
    buf))

;; region

(defun cfw:insert-calendar-region (&optional date width height custom-map)
  "Insert markers of the rendering destination at current point and display the calendar view.

This function returns the destination object and stores it at the text property `cfw:dest'.

DATE is initial focus date. If it is nil, today is selected
initially.  WIDTH and HEIGHT are reference size of the calendar
view.  If those are nil, the size is calculated from the selected
window. CUSTOM-MAP is the keymap that is put to the text property
`keymap'."
  (let (mark-begin mark-end dest)
    (setq mark-begin (point-marker))
    (insert " ")
    (setq mark-end (point-marker))
    (save-excursion
      (setq dest (cfw:dest-init-region (current-buffer) mark-begin mark-end width height))
      (setf (cfw:dest-update-func dest) 
            (lexical-let ((custom-map custom-map) (dest dest))
              (lambda () 
                (cfw:dest-with-region dest
                  (let (buffer-read-only)
                    (put-text-property (point-min) (point-max) 'cfw:dest dest)
                    (put-text-property (point-min) (point-max) 'keymap custom-map))))))
      (cfw:calendar-update dest)
      (cfw:navi-goto-date (or date (calendar-current-date))))
    dest))

;; inline

(defun cfw:get-schedule-text (width height &optional date)
  "Return a text that is drew the calendar view.

In this case, the rendering destination object is disposable.

WIDTH and HEIGHT are reference size of the calendar view.  If the
given size is larger than the minimum size (about 45x20), the
calendar is displayed within the given size. If the given size is
smaller, the minimum size is used.

DATE is initial focus date. If it is nil, today is selected
initially."
  (let* ((dest (cfw:dest-init-inline width height))
         (buf (cfw:dest-buffer dest)) text)
    (cfw:calendar-update dest)
    (setq text
          (with-current-buffer buf
            (buffer-substring (point-min) (point-max))))
    (kill-buffer buf)
    text))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level API

;; Buffer

(defconst cfw:calendar-buffer-name "*cfw-calendar*" "[internal] Default buffer name for the calendar view.")

(defun cfw:dest-init-buffer (&optional buf width height custom-map)
  "Create a buffer destination.
This destination uses an entire buffer and set up the major-mode
`cfw:calendar-mode' and the key map `cfw:calendar-mode-map'.  BUF
is a buffer name to render the calendar view. If BUF is nil, the
default buffer name `cfw:calendar-buffer-name' is used.  WIDTH
and HEIGHT are reference size of the calendar view. If those are
nil, the size of calendar is calculated from the window that
shows BUF or the selected window.  The rendering destination
object is stored at the buffer local variable `cfw:dest'.
CUSTOM-MAP is the additional keymap that is added to default
keymap `cfw:calendar-mode-map'."
  (lexical-let
      ((buffer (or buf (get-buffer-create cfw:calendar-buffer-name)))
       (window (or (and buf (get-buffer-window buf)) (selected-window)))
       dest)
    (setq dest
          (make-cfw:dest
           :type 'buffer
           :min-func 'point-min
           :max-func 'point-max
           :buffer buffer
           :width (or width (window-width window))
           :height (or height (window-height window))
           :clear-func (lambda () 
                         (with-current-buffer buffer
                           (erase-buffer)))))
    (with-current-buffer buffer
      (unless (eq major-mode 'cfw:calendar-mode)
        (cfw:calendar-mode custom-map))
      (set (make-local-variable 'cfw:dest) dest))
    dest))

;; Region

(defun cfw:dest-init-region (buf mark-begin mark-end &optional width height)
  "Create a region destination.  The calendar is drew between
MARK-BEGIN and MARK-END in the buffer BUF.  MARK-BEGIN and
MARK-END are separated by more than one character, such as a
space.  This destination is employed to be embedded in the some
application buffer.  Because this destination does not set up
any modes and key maps for the buffer, the application that uses
the calfw is responsible to manage the buffer and key maps."
  (lexical-let
      ((mark-begin mark-begin) (mark-end mark-end)
       (window (or (get-buffer-window buf) (selected-window))))
    (make-cfw:dest
     :type 'region
     :min-func (lambda () (marker-position mark-begin))
     :max-func (lambda () (marker-position mark-end))
     :buffer buf
     :width (or width (window-width window))
     :height (or height (window-height window))
     :clear-func 
     (lambda () 
         (cfw:dest-region-clear (marker-position mark-begin) 
                                (marker-position mark-end)))
     )))

(defun cfw:dest-region-clear (begin end)
  (when (< 2 (- end begin))
    (delete-region begin (1- end)))
  (goto-char begin))

;; Inline text

(defconst cfw:dest-background-buffer " *cfw:dest-background*")

(defun cfw:dest-init-inline (width height)
  "Create a text destination."
  (lexical-let
      ((buffer (get-buffer-create cfw:dest-background-buffer))
       (window (selected-window))
       dest)
    (setq dest
          (make-cfw:dest
           :type 'text
           :min-func 'point-min
           :max-func 'point-max
           :buffer buffer
           :width (or width (window-width window))
           :height (or height (window-height window))
           :clear-func (lambda () 
                         (with-current-buffer buffer
                           (erase-buffer)))))
    dest))


;;; View

(defun cfw:calendar-get-dest ()
  "Return the rendering destination object on the current cursor position.
Firstly, getting a text property `cfw:dest' on the current
position. Secondly, getting a buffer local variable
`cfw:dest'. If no object is found, return nil."
  (let ((dest (get-text-property (point) 'cfw:dest)))
    (unless dest
      (setq dest (buffer-local-value 'cfw:dest (current-buffer))))
    dest))

(defvar cfw:calendar-update-after-hook nil "List of functions called whenever updating the calendar.")

(defun cfw:calendar-update (dest &optional month year)
  "Update the rendering destination DEST with MONTH and YEAR."
  (let* ((today (calendar-current-date))
         (month (or month (calendar-extract-month today)))
         (year (or year (calendar-extract-year today)))
         (buf (cfw:dest-buffer dest)) model)
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (cfw:dest-with-region dest 
          (cfw:navi-selection-clear)
          (cfw:dest-clear dest)
          (cfw:render-overlays-clear)
          (setq model (cfw:model-month-create month year))
          (setf (cfw:dest-model dest) model)
          (cfw:render-month model (cfw:render-month-calc-param dest))
          (cfw:render-overlays-put)))
      (run-hooks 'cfw:calendar-update-after-hook)
      (cfw:dest-update dest))))



;;; Rendering

(defvar cfw:render-overlays nil "[internal] Decoration overlays.")
(make-variable-buffer-local 'cfw:render-overlays)

(defun cfw:render-overlays-clear ()
  "[internal] Clear decoration overlays."
  (loop for i in cfw:render-overlays
        do (delete-overlay i))
  (setq cfw:render-overlays nil))

(defun cfw:render-overlays-put ()
  "[internal] Put decoration overlays, such as the today highlight."
  (cfw:find-all-by-date 
   (calendar-current-date)
   (lambda (begin end)
     (let ((overlay (make-overlay begin end)))
       (overlay-put overlay 'face 
                    (if (eq 'cfw:face-day-title 
                            (get-text-property begin 'face))
                        'cfw:face-today-title 'cfw:face-today))
       (add-to-list 'cfw:render-overlays overlay)))))

(defun cfw:render-center (width string &optional padding)
  "[internal] Format STRING in the center, padding on the both
sides with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string 
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (/ (- width len) 2)))
    (concat 
     (make-string margin padding) cnt
     (make-string (- width len margin) padding))))

(defun cfw:render-left (width string &optional padding)
  "[internal] Format STRING, padding on the right with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string 
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat cnt (make-string margin padding))))

(defun cfw:render-right (width string &optional padding)
  "[internal] Format STRING, padding on the left with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string 
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat (make-string margin padding) cnt)))

(defun cfw:render-add-right (width left right &optional padding)
  "[internal] Layout strings LEFT and RIGHT within WIDTH."
  (let* ((padding (or padding ?\ ))
         (lcnt (or (and left 
                        (cfw:render-truncate left width t))
                   ""))
         (llen (string-width lcnt))
         (rmargin (- width llen))
         (right (cfw:trim right))
         (rcnt (or (and right (> rmargin 0)
                        (cfw:render-truncate right rmargin))
                   ""))
         (cmargin (- width llen (string-width rcnt))))
    (concat lcnt (if (< 0 cmargin) (make-string cmargin padding)) rcnt)))

(defun cfw:render-month-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((win-width (cfw:dest-width dest))
       (win-height (max 15 (- (cfw:dest-height dest) 16)))
       (cell-width  (max 5 (/ (- win-width 8) 7)))
       (cell-height (max 2 (/ (- win-height 6) 5)))
       (total-width (+ (* cell-width cfw:week-days) 8)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width))))

(defun cfw:render-month (model param)
  "[internal] Render monthly calendar view. MODEL is a logical
data. PARAM is an alist of the rendering parameters."
  (let* ((cell-width  (cfw:k 'cell-width  param))
         (cell-height (cfw:k 'cell-height param))
         (total-width (cfw:k 'total-width param))
         (EOL "\n") (VL (cfw:rt "|" 'cfw:face-grid))
         (hline (cfw:rt (concat (make-string total-width ?-) EOL) 'cfw:face-grid))
         (cline (cfw:rt (concat 
                         (loop for i from 0 below cfw:week-days
                               concat (concat "+" (make-string cell-width ?-)))
                         "+" EOL) 'cfw:face-grid)))
    ;; header
    (insert
     (cfw:rt (format "%4s / %s" 
                     (cfw:k 'year model)
                     (aref calendar-month-name-array (1- (cfw:k 'month model))))
             'cfw:face-title)
     EOL hline)
    ;; day names
    (loop for i in (cfw:k 'headers model)
          for name = (aref calendar-day-name-array i) do
          (insert VL (cfw:rt (cfw:render-center cell-width name) 
                              (cfw:render-get-week-face i 'cfw:face-header))))
    (insert VL EOL cline)
    ;; contents
    (loop for week in (cfw:k 'weeks model) ; week rows loop 
          with month       = (cfw:k 'month    model) 
          with year        = (cfw:k 'year     model)
          with headers     = (cfw:k 'headers  model) 
          with holidays    = (cfw:k 'holidays model)
          with contents    = (cfw:k 'contents model)
          with annotations = (cfw:k 'annotations model)
          with periods     = (cfw:render-layout-periods model)
          do
          (cfw:render-month-week
           (loop for day in week ; week columns loop 
                 for count from 0 below (length week)
                 for week-day = (nth count headers)
                 for date = (cfw:date month day year)
                 for hday = (car (cfw:contents-get date holidays))
                 for ant = (cfw:rt (cfw:contents-get date annotations) 'cfw:face-annotation)
                 for raw-periods = (cfw:contents-get date periods)
                 for raw-contents = (cfw:contents-get date contents)
                 for prs-contents = (append
                                     (cfw:render-periods date week-day raw-periods)
                                     (mapcar 'cfw:render-default-content-face raw-contents))
                 for num-label = (if prs-contents
                                     (format "(%s)" 
                                             (+ (length raw-contents)
                                                (length raw-periods))) "")
                 for tday = (concat
                             " "
                             (cfw:rt (format "%s" (or day ""))
                                     (if hday 'cfw:face-sunday 
                                       (cfw:render-get-week-face 
                                        week-day 'cfw:face-default-day)))
                             (if num-label (concat " " num-label))
                             (if hday (concat " " (cfw:rt (substring hday 0) 'cfw:face-holiday))))
                 collect
                 (cons date (cons (cons tday ant) prs-contents)))))))

(defun cfw:render-month-week (week-days)
  "[internal] This function is an internal function of `cfw:render-month',
then, uses some local variables in `cfw:render-month' as readonly ones.
This function concatenates each rows on the days into a string of a physical line."
  (loop for day-rows in week-days
        for date = (car day-rows)
        for (tday . ant) = (cadr day-rows)
        do
        (insert
         VL (if date
                (cfw:tp 
                 (cfw:render-default-content-face
                  (cfw:render-add-right cell-width tday ant)
                  'cfw:face-day-title)
                 'cfw:date date)
              (cfw:render-left cell-width ""))))
  (insert VL EOL)
  (loop for i from 2 upto cell-height do
        (loop for day-rows in week-days
              for date = (car day-rows)
              for row = (nth i day-rows)
              do
              (insert
               VL (cfw:tp 
                    (cfw:render-left cell-width (and row (format "%s" row)))
                    'cfw:date date)))
        (insert VL EOL))
  (insert cline))

(defun cfw:render-default-content-face (str &optional default-face)
  "[internal] Put the default content face. If STR has some
faces, the faces are remained."
  (loop for i from 0 below (length str)
        with ret = (substring str 0)
        with face = (or default-face 'cfw:face-default-content)
        unless (get-text-property i 'face ret)
        do 
        (put-text-property i (1+ i) 'face face ret)
        (put-text-property i (1+ i) 'font-lock-face face ret)
        finally return ret))

(defun cfw:render-get-week-face (daynum &optional default-face)
  "[internal] Put the default week face."
  (cond
   ((= daynum cfw:week-saturday)
    'cfw:face-saturday)
   ((= daynum cfw:week-sunday)
    'cfw:face-sunday)
   (t default-face)))

(defun cfw:render-truncate (org limit-width &optional ellipsis)
  "[internal] Truncate a string ORG with LIMIT-WIDTH, like `truncate-string-to-width'."
  (if (< limit-width (string-width org))
      (let ((str (truncate-string-to-width 
                  (substring org 0) limit-width 0 nil ellipsis)))
        (cfw:tp str 'mouse-face 'highlight)
        (cfw:tp str 'help-echo org)
        str)
    org))

(defun cfw:render-periods (date week-day periods-stack)
  "[internal] This function is an internal function of `cfw:render-month', then,
uses some local variables in `cfw:render-month' as readonly ones.
This function translates PERIOD-STACK to display content on the DATE."
  (when periods-stack
    (let ((stack (sort periods-stack (lambda (a b) (< (car a) (car b))))))
      (loop for i from 0 below (car (car stack))
            do (push ; insert blank lines
                (list i (list nil nil nil))
                stack))
      (loop for (row (begin end content)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width (if beginp 1 0) (if endp 1 0))
            for title = (if (and content 
                                 (or (equal date begin)
                                     (eql 1 (calendar-extract-day date))
                                     (eql week-day calendar-week-start-day)))
                            (cfw:render-truncate content width t) "")
            collect
            (if content
                (cfw:rt
                 (concat 
                  (if beginp "(" "")
                  (cfw:render-left width title ?-)
                  (if endp ")" ""))
                 'cfw:face-periods)
              "")))))

(defun cfw:render-layout-periods-get-min (periods-each-days begin end)
  "[internal] Find the minimum empty row number of the days between
BEGIN and END from the PERIODS-EACH-DAYS."
  (loop for row-num from 0 below 10 ; assuming the number of stacked periods is less than 10
        unless
        (loop for d in (cfw:enumerate-days begin end)
              for periods-stack = (cfw:contents-get d periods-each-days)
              if (and periods-stack (assq row-num periods-stack))
              return t)
        return row-num))

(defun cfw:render-layout-periods-place (periods-each-days row period)
  "[internal] Assign PERIOD content to the ROW-th row on the days of the period,
and append the result to periods-each-days."
  (loop for d in (cfw:enumerate-days (car period) (cadr period))
        for periods-stack = (cfw:contents-get d periods-each-days)
        if periods-stack
        do (nconc periods-stack (list (list row period)))
        else
        do (push (cons d (list (list row period))) periods-each-days))
  periods-each-days)

(defun cfw:render-layout-periods (model)
  "[internal] Arrange the `periods' records of the model and
create period-stacks on the each days. 
period-stack -> ((row-num . period) ... )"
  (let* (periods-each-days)
    (loop for period in (cfw:k 'periods model)
          for (begin end content) = period
          for row = (cfw:render-layout-periods-get-min
                     periods-each-days begin end)
          do 
          (setq periods-each-days
                (cfw:render-layout-periods-place
                 periods-each-days row period)))
    periods-each-days))



;;; Models

;; public functions

(defun cfw:model-get-holiday-by-date (date model)
  "Return a holiday title on the DATE."
  (cfw:contents-get date (cfw:k 'holidays model)))

(defun cfw:model-get-contents-by-date (date model)
  "Return a list of contents on the DATE."
  (cfw:contents-get date (cfw:k 'contents model)))

(defun cfw:model-get-annotation-by-date (date model)
  "Return an annotation on the DATE."
  (cfw:contents-get date (cfw:k 'annotations model)))

(defun cfw:model-get-periods-by-date (date model)
  "Return a list of periods on the DATE."
  (loop for period in (cfw:k 'periods model)
        for (begin end content) = period
        if (cfw:date-between begin end date)
        collect period))

;; private functions

(defun cfw:model-month-create (month year)
  "[internal] Create a logical view model of monthly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((day-names 
          (loop for i from 0 below cfw:week-days 
                collect (% (+ calendar-week-start-day i) cfw:week-days)))
         (last-month-day (calendar-last-day-of-month month year))
         (first-day-day (calendar-day-of-week (cfw:date month 1 year)))
         (holidays (let ((displayed-month month)
                         (displayed-year year))
                     (calendar-holiday-list)))
         (begin-date (cfw:date month 1 year)) 
         (end-date (cfw:date month last-month-day year))
         (contents-all (cfw:contents-merge begin-date end-date))
         (contents (loop for i in contents-all 
                         unless (eq 'periods (car i)) 
                         collect i))
         weeks)
    ;; making 'weeks'
    (loop with i = (+ (- 1 first-day-day) calendar-week-start-day)
          with day = calendar-week-start-day
          with week = nil
          do
          ;; flush a week
          (when (and (= day calendar-week-start-day) week)
            (push (nreverse week) weeks)
            (setq week nil)
            (when (< last-month-day i) (return)))
          ;; add a day
          (push (if (and (< 0 i) (<= i last-month-day)) i nil) week)
          ;; increment
          (setq day (% (1+ day) cfw:week-days))
          (incf i))
    ;; model
    `((month . ,month)       ; a number of month (it begins from 1)
      (year . ,year)         ; a number of year
      (headers . ,day-names) ; a list of the index of day-of-week
      (holidays . ,holidays) ; an alist of holidays, (DATE HOLIDAY-NAME)
      (annotations . ,(cfw:annotations-merge begin-date end-date)) ; an alist of annotations, (DATE ANNOTATION)
      (contents . ,contents) ; an alist of contents, (DATE LIST-OF-CONTENTS)
      (periods . ,(cfw:k 'periods contents-all)) ; a list of periods, (BEGIN-DATE END-DATE SUMMARY)
      (weeks . ,(nreverse weeks)) ; a matrix of day-of-month, which corresponds to the index of `headers'
      )))

(defun cfw:contents-get (date contents)
  "[internal] Return a list of contents on the DATE."
  (cdr (cfw:contents-get-internal date contents)))

(defun cfw:contents-get-internal (date contents)
  "[internal] Return a cons cell that has the key DATE.
One can modify the returned cons cell destructively."
  (cond
   ((or (null date) (null contents)) nil)
   (t (loop for i in contents
            if (equal date (car i))
            return i
            finally return nil))))

(eval-when-compile
  (defmacro cfw:contents-add (date content contents)
    "[internal] Add a record, DATE as a key and CONTENT as a
body, to CONTENTS. If CONTENTS has a record for DATE, this macro
appends CONTENT to the record."
    (let (($prv (gensym)) ($lst (gensym))
          ($d (gensym)) ($c (gensym)))
      `(let* ((,$d ,date) (,$c ,content)
              (,$prv (cfw:contents-get-internal ,$d ,contents))
              (,$lst (if (listp ,$c) (cfw:copy-list ,$c) (list ,$c))))
         (if ,$prv (nconc ,$prv ,$lst)
           (push (cons ,$d ,$lst) ,contents))))))

(defun cfw:contents-merge (begin end)
  "[internal] Return an contents alist between begin date and end one,
calling functions `cfw:contents-functions'."
  (cond 
   ((null cfw:contents-functions) nil)
   ((= 1 (length cfw:contents-functions))
    (funcall (car cfw:contents-functions) begin end))
   (t
    (loop for f in cfw:contents-functions
          for cnts = (funcall f begin end)
          with contents = nil
          do
          (loop for c in cnts
                for (d . line) = c
                do (cfw:contents-add d line contents))
          finally return contents))))

(defun cfw:annotations-merge (begin end)
  "[internal] Return an annotation alist between begin date and end one,
calling functions `cfw:annotations-functions'."
  (cond 
   ((null cfw:annotations-functions) nil)
   ((= 1 (length cfw:annotations-functions))
    (funcall (car cfw:annotations-functions) begin end))
   (t
    (loop for f in cfw:annotations-functions
          for cnt = (funcall f begin end)
          with annotations = nil
          for prv = (cfw:contents-get-internal d annotations)
          if prv
          do (set-cdr prv (concat (cdr prv) "/" (cdr cnt)))
          else
          do (push (cfw:copy-list cnt) annotations)
          finally return annotations))))

(defun cfw:contents-debug-data ()
  (setq cfw:contents-functions
        (list
         (lambda (b e)
           '(((1  1 2011) "TEST1") 
             ((1 10 2011) "TEST2" "TEST3")
             (periods 
              ((1 8 2011) (1 9 2011) "PERIOD1")
              ((1 11 2011) (1 12 2011) "Period2")
              ((1 12 2011) (1 14 2011) "long long title3"))
             ))
         (lambda (b e) 
           '(((1  2 2011) "PTEST1") 
             ((1 10 2011) "PTEST2" "PTEST3")
             (periods 
              ((1 14 2011) (1 15 2011) "Stack")
              ((1 29 2011) (1 31 2011) "PERIOD W"))
             ))))
  (setq cfw:annotations-functions
        (list
         (lambda (b e)
           '(((1  4 2011) . "New Moon") 
             ((1 12 2011) . "Young Moon")
             ((1 20 2011) . "Full Moon")
             ((1 26 2011) . "Waning Moon")
             )))))



;;; Navigation

;; Following functions assume that the current buffer is a calendar view.

(defun cfw:cursor-to-date (&optional pos)
  "[internal] Return the date at the cursor. If the text does not
have the text-property `cfw:date', return nil."
  (get-text-property (or pos (point)) 'cfw:date))

(defun cfw:cursor-to-nearest-date ()
  "Return the date at the cursor. If the point of cursor does not
have the date, search the date around the cursor position. If the
current buffer is not calendar view (it may be bug), this
function may return nil."
  (or (cfw:cursor-to-date)
      (let* ((r (lambda () (when (not (eolp)) (forward-char))))
             (l (lambda () (when (not (bolp)) (backward-char))))
             (u (lambda () (when (not (bobp)) (line-move 1))))
             (d (lambda () (when (not (eobp)) (line-move -1)))) get)
        (setq get (lambda (cmds)
                    (save-excursion
                      (if (null cmds) (cfw:cursor-to-date)
                        (ignore-errors
                          (funcall (car cmds)) (funcall get (cdr cmds)))))))
        (or (loop for i in `((,d) (,r) (,u) (,l)
                             (,d ,r) (,d ,l) (,u ,r) (,u ,l)
                             (,d ,d) (,r ,r) (,u ,u) (,l ,l))
                  for date = (funcall get i)
                  if date return date)
            (cond
             ((> (/ (point-max) 2) (point))
              (cfw:find-first-date))
             (t (cfw:find-last-date)))))))

(defun cfw:find-first-date ()
  "[internal] Return the first date in the current buffer."
  (let ((pos (next-single-property-change (point-min) 'cfw:date)))
    (and pos (cfw:cursor-to-date pos))))

(defun cfw:find-last-date ()
  "[internal] Return the last date in the current buffer."
  (let ((pos (previous-single-property-change (point-max) 'cfw:date)))
    (and pos (cfw:cursor-to-date (1- pos)))))

(defun cfw:find-by-date (date)
  "[internal] Return a point where the text property `cfw:date'
is equal to DATE in the current calender view. If DATE is not
found in the current view, return nil."
  (let ((pos (point-min)) begin ret)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq pos begin
            text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (setq ret begin
              pos (point-max))))
    ret))

(defun cfw:find-all-by-date (date func)
  "[internal] Call the function FUNC in each regions where the
text-property `cfw:date' is equal to DATE. The function FUNC
receives two arguments, begin date and end one. This function is
mainly used at functions for putting overlays."
  (let ((pos (point-min)) begin text-date)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (let ((end (next-single-property-change 
                    begin 'cfw:date nil (point-max))))
          (funcall func begin end)))
      (setq pos begin))))

(defvar cfw:move-hook nil "List of functions called whenever the cursor moves in the calendar. The functions receive one argument, DATE.")
(make-variable-buffer-local 'cfw:navi-selection-overlays)

(defun cfw:navi-goto-date-internal (date)
  "[internal] Move the cursor to DATE on the current calendar
view and put selection."
  (let ((pos (cfw:find-by-date date)))
    (goto-char pos)
    (unless (eql (selected-window) (get-buffer-window (current-buffer)))
      (set-window-point (get-buffer-window (current-buffer)) pos)))
  (cfw:navi-selection-clear)
  (cfw:navi-selection-set date)
  (run-hook-with-args 'cfw:move-hook date))

(defvar cfw:navi-selection-overlays nil "[internal] Selection overlays in the current buffer.")
(make-variable-buffer-local 'cfw:navi-selection-overlays)

(defun cfw:navi-selection-clear ()
  "Clear the selection overlays on the current calendar view."
  (loop for i in cfw:navi-selection-overlays
        do (delete-overlay i))
  (setq cfw:navi-selection-overlays nil))

(defun cfw:navi-selection-set (date)
  "Put a selection overlay on DATE. The selection overlay can be
 put on some days, calling this function many times.  If DATE is
 not included on the current calendar view, do nothing. This
 function does not manage the selections, just put the overlay."
  (cfw:find-all-by-date 
   date
   (lambda (begin end) 
     (let ((overlay (make-overlay begin end)))
       (overlay-put overlay 'face 
                    (if (eq 'cfw:face-day-title 
                            (get-text-property begin 'face))
                        'cfw:face-select))
       (add-to-list 'cfw:navi-selection-overlays overlay)))))

(defun cfw:navi-goto-date (date)
  "Move the cursor to DATE and put selection. If DATE is not
included on the current calendar, this function changes the
calendar view."
  (unless (cfw:find-by-date date)
    (cfw:calendar-update (cfw:calendar-get-dest)
                         (calendar-extract-month date)
                         (calendar-extract-year date)))
  (cfw:navi-goto-date-internal date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major Mode / Key bindings 

(defvar cfw:calendar-mode-map
  (cfw:define-keymap
   '(
     ("<right>" . cfw:navi-next-day-command)
     ("<left>"  . cfw:navi-previous-day-command)
     ("<down>"  . cfw:navi-next-week-command)
     ("<up>"    . cfw:navi-previous-week-command)

     ;; Emacs style
     ("C-f"   . cfw:navi-next-day-command)
     ("C-b"   . cfw:navi-previous-day-command)
     ("C-n"   . cfw:navi-next-week-command)
     ("C-p"   . cfw:navi-previous-week-command)
     ("C-a" . cfw:navi-goto-week-begin-command)
     ("C-e" . cfw:navi-goto-week-end-command)
     ;; Vi style
     ("l" . cfw:navi-next-day-command)
     ("h" . cfw:navi-previous-day-command)
     ("j" . cfw:navi-next-week-command)
     ("k" . cfw:navi-previous-week-command)
     ("^" . cfw:navi-goto-week-begin-command)
     ("$" . cfw:navi-goto-week-end-command)

     ("<" . cfw:navi-previous-month-command)
     (">" . cfw:navi-next-month-command)
     ("<prior>" . cfw:navi-previous-month-command)
     ("<next>"  . cfw:navi-next-month-command)
     ("<home>" . cfw:navi-goto-first-date-command)
     ("<end>"  . cfw:navi-goto-last-date-command)

     ("r" . cfw:refresh-calendar-buffer)
     ("SPC" . cfw:show-details-command)

     ("g" . cfw:navi-goto-date-command)
     ("t" . cfw:navi-goto-today-command)))
  "Default key map of calendar views.")

(defun cfw:calendar-mode-map (&optional custom-map)
  (cond
   (custom-map
    (set-keymap-parent custom-map cfw:calendar-mode-map)
    custom-map)
   (t cfw:calendar-mode-map)))

(defvar cfw:calendar-mode-hook nil
  "This hook is called at end of setting up major mode `cfw:calendar-mode'.")

(defun cfw:calendar-mode (&optional custom-map)
  "Set up major mode `cfw:calendar-mode'."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map (cfw:calendar-mode-map custom-map))
  (setq major-mode 'cfw:calendar-mode
        mode-name "Calendar Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'cfw:calendar-mode-hook))

;;; Actions

(defun cfw:refresh-calendar-buffer ()
  "Clear the calendar and render again."
  (interactive)
  (when cfw:dest
    (let ((date (or (cfw:cursor-to-nearest-date) 
                            (calendar-current-date))))
      (cfw:calendar-update cfw:dest)
      (cfw:navi-goto-date date))))

(defun cfw:navi-goto-week-begin-command ()
  "Move the cursor to the first day of the current week."
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (back-num (% (- (calendar-day-of-week cursor-date) 
                         calendar-week-start-day)
                      cfw:week-days)))
    (cfw:navi-previous-day-command back-num)))

(defun cfw:navi-goto-week-end-command ()
  "Move the cursor to the last day of the current week."
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (forward-num (% (- cfw:week-saturday (calendar-day-of-week cursor-date)
                            calendar-week-start-day)
                         cfw:week-days)))
    (cfw:navi-next-day-command forward-num)))

(defun cfw:navi-goto-date-command (string-date)
  "Move the cursor to the specified date."
  (interactive "sInput Date (YYYY/MM/DD): ")
  (cfw:navi-goto-date (cfw:parsetime string-date)))

(defun cfw:navi-goto-today-command ()
  "Move the cursor to today."
  (interactive)
  (cfw:navi-goto-date (cfw:emacs-to-calendar (current-time))))

(defun cfw:navi-next-day-command (&optional num)
  "Move the cursor forward NUM days. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive)
  (unless num (setq num 1))
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (new-cursor-date
          (calendar-gregorian-from-absolute
           (+ (calendar-absolute-from-gregorian cursor-date) num))))
    (cfw:navi-goto-date new-cursor-date)))

(defun cfw:navi-previous-day-command (&optional num)
  "Move the cursor back NUM days. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (- (or num 1))))

(defun cfw:navi-goto-first-date-command ()
  "Move the cursor to the first day on the current calendar view."
  (interactive)
  (cfw:navi-goto-date (cfw:find-first-date)))

(defun cfw:navi-goto-last-date-command ()
  "Move the cursor to the last day on the current calendar view."
  (interactive)
  (cfw:navi-goto-date (cfw:find-last-date)))

(defun cfw:navi-next-week-command (&optional num)
  "Move the cursor forward NUM weeks. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (* cfw:week-days (or num 1))))

(defun cfw:navi-previous-week-command (&optional num)
  "Move the cursor back NUM weeks. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive)
  (cfw:navi-next-day-command (* (- cfw:week-days) (or num 1))))

(defun cfw:navi-next-month-command (&optional num)
  "Move the cursor forward NUM months. If NUM is nil, 1 is used.
Movement is backward if NUM is negative."
  (interactive)
  (unless num (setq num 1))
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (month (calendar-extract-month cursor-date))
         (day   (calendar-extract-day   cursor-date))
         (year  (calendar-extract-year  cursor-date))
         (last (progn
                 (calendar-increment-month month year num)
                 (calendar-last-day-of-month month year)))
         (day (min last day))
         (new-cursor-date (cfw:date month day year)))
    (cfw:navi-goto-date new-cursor-date)))

(defun cfw:navi-previous-month-command (&optional num)
  "Move the cursor back NUM months. If NUM is nil, 1 is used.
Movement is forward if NUM is negative."
  (interactive)
  (cfw:navi-next-month-command (- (or num 1))))

;;; Detail popup

(defun cfw:show-details-command ()
  "Show details on the selected date."
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (dest  (cfw:calendar-get-dest))
         (model (and dest (cfw:dest-model dest))))
    (when model
      (cfw:details-popup
       (cfw:details-layout cursor-date model)))))

(defvar cfw:details-buffer-name "*cfw:details*" "[internal]")
(defvar cfw:details-window-size 20 "Default detail buffer window size.")

(defun cfw:details-popup (text)
  "Popup the buffer to show details.
TEXT is a content to show."
  (let ((buf (get-buffer cfw:details-buffer-name))
        (before-win-num (length (window-list)))
        (main-buf (current-buffer)))
    (unless (and buf (eq (buffer-local-value 'major-mode buf)
                         'cfw:details-mode))
      (setq buf (get-buffer-create cfw:details-buffer-name))
      (with-current-buffer buf
        (cfw:details-mode)
        (set (make-local-variable 'cfw:before-win-num) before-win-num)))
    (with-current-buffer buf
      (let (buffer-read-only)
        (set (make-local-variable 'cfw:main-buf) main-buf)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))
    (pop-to-buffer buf)
    (fit-window-to-buffer (get-buffer-window buf) cfw:details-window-size)))

(defun cfw:details-layout (date model)
  "Layout details and return the text.
DATE is a date to show. MODEL is model object."
  (let* ((EOL "\n") 
         (HLINE (cfw:rt (concat (make-string (window-width) ?-) EOL) 'cfw:face-grid))
         (holiday (cfw:model-get-holiday-by-date date model))
         (annotation (cfw:model-get-annotation-by-date date model))
         (periods (cfw:model-get-periods-by-date date model))
         (contents (cfw:model-get-contents-by-date date model)))
  (concat 
   (cfw:rt (concat "Schedule on " (cfw:strtime date) " (") 'cfw:face-header)
   (cfw:rt (calendar-day-name date) 
           (cfw:render-get-week-face (calendar-day-of-week date) 'cfw:face-header))
   (cfw:rt (concat ")" EOL) 'cfw:face-header)
   (when (or holiday annotation) 
     (concat 
      (and holiday (cfw:rt holiday 'cfw:face-holiday))
      (and holiday annotation " / ")
      (and annotation (cfw:rt annotation 'cfw:face-annotation))
      EOL))
   HLINE
   (loop for (begin end summary) in periods concat
         (concat
          (cfw:rt (concat 
                   (cfw:strtime begin) " - " (cfw:strtime end) " : ") 
                  'cfw:face-periods)
          " " summary EOL))
   (loop for i in contents concat
         (concat "- " i EOL)))))

(defvar cfw:details-mode-map
  (cfw:define-keymap
   '(("q" . cfw:details-kill-buffer-command)
     ("n" . cfw:details-navi-next-command)
     ("p" . cfw:details-navi-prev-command)
     ))
  "Default key map for the details buffer.")

(defvar cfw:details-mode-hook nil "")

(defun cfw:details-mode ()
  "Set up major mode `cfw:details-mode'."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map cfw:details-mode-map)
  (setq major-mode 'cfw:details-mode
        mode-name "Calendar Details Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'cfw:details-mode-hook))

(defun cfw:details-kill-buffer-command ()
  "Kill buffer and delete window."
  (interactive)
  (let ((win-num (length (window-list)))
        (next-win (get-buffer-window cfw:main-buf)))
    (when (and (not (one-window-p))
               (> win-num cfw:before-win-num))
      (delete-window))
    (kill-buffer cfw:details-buffer-name)
    (when next-win (select-window next-win))))

(defun cfw:details-navi-next-command (&optional num)
  "details-navi-next"
  (interactive)
  (when cfw:main-buf
    (with-current-buffer cfw:main-buf
      (cfw:navi-next-day-command num)
      (cfw:show-details-command))))

(defun cfw:details-navi-prev-command (&optional num)
  "details-navi-prev"
  (interactive)
  (when cfw:main-buf
    (with-current-buffer cfw:main-buf
      (cfw:navi-previous-day-command num)
      (cfw:show-details-command))))

(provide 'calfw)
;;; calfw.el ends here

;; (cfw:contents-debug-data)
;; (progn (eval-current-buffer) (cfw:open-calendar-buffer))