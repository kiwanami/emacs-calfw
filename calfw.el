;;; calfw.el --- Calendar view framework -*- lexical-binding: t -*-

;; Copyright (C) 2011-2021  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Version: 2.0
;; Keywords: calendar
;; Package-Requires: ((emacs "28.1"))
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

;; This program is a framework for the Calendar component.  In the
;; Emacs, uses can show schedules in the calendar views, like iCal,
;; Outlook and Google Calendar.

;;; Installation:

;; Place this program in your load path and add following code.

;; (require 'calfw)

;;; Usage:

;; Executing the command `calfw-open-calendar-buffer', switch to the calendar buffer.
;; You can navigate the date like calendar.el.

;; Schedule data which are shown in the calendar view, are collected
;; by the `calfw-source' objects. See the function `calfw-open-debug-calendar' for example.

;; This program gets the holidays using the function
;; `calendar-holiday-list'. See the document of the holidays.el and
;; the Info text for customizing the holidays.

;;; Add-ons:

;; - calfw-howm.el : Display howm schedules.
;; - calfw-ical.el : Display schedules of the iCalendar format.
;; - calfw-org.el  : Display orgmode schedules.
;; - calfw-cal.el  : Display emacs diary schedules.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'holidays)
(require 'format-spec)



;;; Constants

(defconst calfw-week-sunday    0)
(defconst calfw-week-monday    1)
(defconst calfw-week-tuesday   2)
(defconst calfw-week-wednesday 3)
(defconst calfw-week-thursday  4)
(defconst calfw-week-friday    5)
(defconst calfw-week-saturday  6)
(defconst calfw-week-days      7)

;;; Customs

(defcustom calfw-fchar-vertical-line ?|
  "The character used for drawing vertical lines."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-horizontal-line ?-
  "The character used for drawing horizontal lines."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-junction ?+
  "The character used for drawing junction lines."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-top-right-corner ?+
  "The character used for drawing the top-right corner."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-top-left-corner ?+
  "The character used for drawing the top-left corner."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-left-junction ?+
  "The character used for drawing junction lines at the left side."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-right-junction ?+
  "The character used for drawing junction lines at the right side."
  :group 'calfw
  :type 'character)

(defcustom calfw-fchar-top-junction ?+
  "The character used for drawing junction lines at the top side."
  :group 'calfw
  :type 'character)

(defcustom calfw-fstring-period-start "("
  "The string used to indicate the beginning of a period."
  :group 'calfw
  :type 'string)

(defcustom calfw-fstring-period-end ")"
  "The string used to indicate the end of a period."
  :group 'calfw
  :type 'string)

(defcustom calfw-read-date-command 'calfw-read-date-command-simple
  "The command used to read the date in `calfw-navi-goto-date-command'.
For example `calfw-read-date-command-simple' or `calfw-org-read-date-command'."
  :group 'calfw
  :type 'function)

(defcustom calfw-event-format-overview "%t"
  "Format string of `calfw-event's for overviews (month-, 2-week-, week-view).
See `calfw-event-format' for possible values."
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-days-overview "%s%e%t"
  "Format string of `calfw-event's for days overviews.
See `calfw-event-format' for possible values."
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-period-overview "%t%l"
  "Format string of `calfw-event's for period overviews.
See `calfw-event-format' for possible values."
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-detail "%s%e%t%l%d"
  "Format string of `calfw-event's for overviews (month-, week-, day-view).
See `calfw-event-format' for possible values."
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-title "%s"
  "Format string for the title of a `calfw-event'.
%s = title string"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-start-date "%Y-%m-%d"
  "Format string for the start date of a `calfw-event'.
%Y = year
%m = month
%d = day"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-start-time "%H:%M "
  "Format string for the start time of a `calfw-event'.
%H = hours
%M = minutes"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-end-date "%Y-%m-%d"
  "Format string for the end date of a `calfw-event'.
%Y = year
%m = month
%d = day"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-end-time "- %H:%M "
  "Format string for the end time of a `calfw-event'.
%H = hours
%M = minutes"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-location "\n  Location:    %s"
  "Format string for the location of a `calfw-event'.
%s = location string"
  :group 'calfw
  :type 'string)

(defcustom calfw-event-format-description "\n\n%s\n--------------------\n"
  "Format string for the description of a `calfw-event'.
%s = location string"
  :group 'calfw
  :type 'string)

(defcustom calfw-display-calendar-holidays t
  "If not-nil, calfw displays holidays."
  :group 'calfw
  :type 'boolean)

;;; Faces

(defface calfw-title-face
  '((((class color) (background light))
     :foreground "DarkGrey" :weight bold :height 2.0 :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "darkgoldenrod3" :weight bold :height 2.0 :inherit variable-pitch)
    (t :height 1.5 :weight bold :inherit variable-pitch))
  "Face for title." :group 'calfw)

(defface calfw-header-face
  '((((class color) (background light))
     :foreground "Slategray4" :background "Gray90" :weight bold)
    (((class color) (background dark))
     :foreground "maroon2" :weight bold))
  "Face for headers." :group 'calfw)

(defface calfw-sunday-face
  '((((class color) (background light))
     :foreground "red2" :background "#ffd5e5" :weight bold)
    (((class color) (background dark))
     :foreground "red" :weight bold))
  "Face for Sunday." :group 'calfw)

(defface calfw-saturday-face
  '((((class color) (background light))
     :foreground "Blue" :background "#d4e5ff" :weight bold)
    (((class color) (background light))
     :foreground "Blue" :weight bold))
  "Face for Saturday." :group 'calfw)

(defface calfw-holiday-face
  '((((class color) (background light))
     :background "#ffd5e5")
    (((class color) (background dark))
     :background "grey10" :foreground "purple" :weight bold))
  "Face for holidays." :group 'calfw)

(defface calfw-grid-face
  '((((class color) (background light))
     :foreground "SlateBlue")
    (((class color) (background dark))
     :foreground "DarkGrey"))
  "Face for grids."
  :group 'calfw)

(defface calfw-default-content-face
  '((((class color) (background light))
     :foreground "#2952a3")
    (((class color) (background dark))
     :foreground "green2"))
  "Face for default contents."
  :group 'calfw)

(defface calfw-periods-face
  '((((class color) (background light))
     :background "#668cd9" :foreground "White" :slant italic)
    (((class color) (background dark))
     :foreground "cyan"))
  "Face for period." :group 'calfw)

(defface calfw-day-title-face
  '((((class color) (background light))
     :background "#f8f9ff")
    (((class color) (background dark))
     :background "grey10"))
  "Face for day title."
  :group 'calfw)

(defface calfw-default-day-face
  '((((class color) (background light))
     :weight bold :inherit calfw-day-title-face)
    (((class color) (background dark))
     :weight bold :inherit calfw-day-title-face))
  "Face for default day." :group 'calfw)

(defface calfw-annotation-face
  '((((class color)) :foreground "RosyBrown" :inherit calfw-day-title-face))
  "Face for annotations."
  :group 'calfw)

(defface calfw-disable-face
  '((((class color)) :foreground "DarkGray" :inherit calfw-day-title-face))
  "Face for days out of focused period."
  :group 'calfw)

(defface calfw-today-title-face
  '((((class color) (background light))
     :background "#fad163")
    (((class color) (background dark))
     :background "red4" :weight bold))
  "Face for today." :group 'calfw)

(defface calfw-today-face
  '((((class color) (background light))
     :background "#fff7d7")
    (((class color) (background dark))
     :foreground "Cyan" :weight bold))
  "Face for today." :group 'calfw)

(defvar calfw-item-separator-color-face "SlateBlue"
  "Color for the separator line of items in a day.")

(defface calfw-calendar-hidden-face
  '((((class color) (background light))
     :foreground "grey"  :strike-through t)
    (((class color) (background dark))
     :foreground "grey" :strike-through t)
    (t :foreground "grey" :strike-through t))
  "Face for calendars when hidden." :group 'calfw)



;;; Utilities

(defun calfw--k (key alist)
  "Get a content by KEY from the given ALIST."
  ;;
  (cdr (assq key alist)))

(defun calfw--sym (&rest strings)
  "Concatenate STRINGS and return as symbol."
  ;;
  (intern-soft (apply 'concat strings)))

(defun calfw--rt (text face)
  "Put a FACE to the given TEXT."
  ;;
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) 'face face text)
  (put-text-property 0 (length text) 'font-lock-face face text)
  text)

(defun calfw--tp (text prop value)
  "Put a text property PROP with VALUE to the entire TEXT."
  ;;
  (unless (stringp text) (setq text (format "%s" text)))
  (when (< 0 (length text))
    (put-text-property 0 (length text) prop value text))
  text)

(defun calfw--extract-text-props (text &rest excludes)
  "Return TEXT properties except those in EXCLUDES."
  ;;
  (cl-loop with ret = nil
           with props = (text-properties-at 0 text)
           for name = (car props)
           for val = (cadr props)
           while props
           do
           (when (and name (not (memq name excludes)))
             (setq ret (cons name (cons val ret))))
           (setq props (cddr props))
           finally return ret))

(defun calfw--define-keymap (keymap-list)
  "Key map definition utility.

KEYMAP-LIST is a source list like ((key . command) ... )."
  ;;
  (let ((new-key-map (make-sparse-keymap)))
    (mapc
     (lambda (i)
       (define-key new-key-map
                   (if (stringp (car i))
                       (read-kbd-macro (car i)) (car i))
                   (cdr i)))
     keymap-list)
    new-key-map))

(defun calfw-flatten (lst &optional revp)
  "Flatten a nested list LST into a single list.
If REVP is non-nil, reverse the order of elements during flattening."
  (cl-loop with ret = nil
           for i in lst
           do (setq ret (if (consp i)
                            (nconc (calfw-flatten i t) ret)
                          (cons i ret)))
           finally return (if revp ret (nreverse ret))))



;;; Date Time Transformation

(defun calfw-date (month day year)
  "Construct a date object in the calendar format, given MONTH, DAY, and YEAR."
  (and month day year
       (list month day year)))

(defun calfw-time (hours minutes)
  "Construct a time object in the calendar format (in local time).

Returns a list of HOURS and MINUTES."
  (and hours minutes
       (list hours minutes)))

(defun calfw-emacs-to-calendar (time)
  "Transform an Emacs TIME format to a calendar one."
  (let ((dt (decode-time time)))
    (list (nth 4 dt) (nth 3 dt) (nth 5 dt))))

(defun calfw-calendar-to-emacs (date)
  "Transform a calendar DATE format to an Emacs format."
  (encode-time 0 0 0
               (calendar-extract-day date)
               (calendar-extract-month date)
               (calendar-extract-year date)))

(defun calfw-month-year-equal-p (date1 date2)
  "Return t if of DATE1 and DATE2 have the same month and year."
  (and
   (= (calendar-extract-month date1)
      (calendar-extract-month date2))
   (= (calendar-extract-year date1)
      (calendar-extract-year date2))))

(defun calfw-date-less-equal-p (d1 d2)
  "Return t if date D1 is less than or equals than date D2."
  (let ((ed1 (calfw-calendar-to-emacs d1))
        (ed2 (calfw-calendar-to-emacs d2)))
    (or (equal ed1 ed2)
        (time-less-p ed1 ed2))))

(defun calfw-date-between (begin end date)
  "Return t if DATE is between BEGIN and END."
  (and (calfw-date-less-equal-p begin date)
       (calfw-date-less-equal-p date end)))

(defun calfw-month-year-contain-p (month year date2)
  "Return t if DATE2 is in MONTH and YEAR."
  (and
   (= month (calendar-extract-month date2))
   (= year (calendar-extract-year date2))))

(defun calfw-date-after (date num)
  "Return the date after NUM days from DATE."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) num)))

(defun calfw-date-before (date num)
  "Return the date before NUM days from DATE."
  (calendar-gregorian-from-absolute
   (- (calendar-absolute-from-gregorian date) num)))

(defun calfw-strtime-emacs (time)
  "Format Emacs time value TIME to the string form YYYY/MM/DD."
  (format-time-string "%Y/%m/%d" time))

(defun calfw-strtime (date)
  "Format calendar date value DATE to the string form YYYY/MM/DD."
  (calfw-strtime-emacs (calfw-calendar-to-emacs date)))

(defun calfw-parsetime-emacs (str)
  "Transform the string format YYYY/MM/DD in STR to an Emacs time value."
  (when (string-match "\\([0-9]+\\)\\/\\([0-9]+\\)\\/\\([0-9]+\\)" str)
    (apply 'encode-time
           (let (ret)
             (dotimes (i 6)
               (push (string-to-number (or (match-string (+ i 1) str) "0")) ret))
             ret))))

(defun calfw-parse-str-time (str)
  "Parse a time string of the format HH:MM in STR to an internal format."
  (when (string-match "\\([[:digit:]]\\{2\\}\\):\\([[:digit:]]\\{2\\}\\)" str)
    (calfw-time (string-to-number (match-string 1 str))
                (string-to-number (match-string 2 str)))))

(defun calfw-parsetime (str)
  "Transform the string format YYYY/MM/DD in STR to a calendar date value."
  (calfw-emacs-to-calendar (calfw-parsetime-emacs str)))


(defun calfw-read-date-command-simple (string-date)
  "Parse STRING-DATE and return it as a calendar date value."
  (interactive "sInput Date (YYYY/MM/DD): ")
  (calfw-parsetime string-date))

(defun calfw-days-diff (begin end)
  "Return the number of days between BEGIN and END."
  (- (time-to-days (calfw-calendar-to-emacs end))
     (time-to-days (calfw-calendar-to-emacs begin))))

(defun calfw-enumerate-days (begin end)
  "Enumerate date objects between BEGIN and END."
  (when (> (calendar-absolute-from-gregorian begin)
           (calendar-absolute-from-gregorian end))
    (error "Invalid period : %S - %S" begin end))
  (let ((d begin) ret (cont t))
    (while cont
      (push (copy-sequence d) ret)
      (setq cont (not (equal d end)))
      (setq d (calfw-date-after d 1)))
    (nreverse ret)))

(defun calfw-week-begin-date (date)
  "Return date of beginning of the week in which DATE is."
  (let ((num (- calendar-week-start-day
                (calendar-day-of-week date))))
    (calfw-date-after date (if (< 0 num) (- num calfw-week-days) num))))

(defun calfw-week-end-date (date)
  "Return date of end of the week in which DATE is."
  (let ((num (+ (- calendar-week-start-day 1)
                (- calfw-week-days (calendar-day-of-week date)))))
    (calfw-date-after date (cond
                            ((> 0 num) (+ num calfw-week-days))
                            ((<= calfw-week-days num) (- num calfw-week-days))
                            (t num)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component

;; This structure defines attributes of the calendar component.
;; These attributes are internal use. Other programs should access
;; through the functions of the component interface.

;; [calfw-component]
;; dest                   : an object of `calfw-dest'
;; model                  : an object of the calendar model
;; view                   : a symbol of view type (month, week, two-weeks, ...)
;; update-hooks           : a list of hook functions for update event
;; click-hooks            : a list of hook functions for click event

(cl-defstruct calfw-component dest model view
              update-hooks click-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Source

;; This structure defines data sources of the calendar.

;; [calfw-source]
;; name   : data source title
;; data   : a function that generates an alist of date-contents
;; update : a function that is called when the user needs to update the contents (optional)
;; color  : foreground color for normal items (optional)
;; period-fgcolor  : foreground color for period items (optional)
;; period-bgcolor  : background color for period items (optional)
;; opt-face        : a plist of additional face properties for normal items (optional)
;; opt-period-face : a plist of additional face properties for period items (optional)
;; hidden  :  non-nil when it should be hidden in the current view
;;
;; If `period-bgcolor' is nil, the value of `color' is used.
;; If `period-fgcolor' is nil, the black or white (negative color of `period-bgcolor') is used.

(cl-defstruct calfw-source name data update color period-bgcolor period-fgcolor opt-face opt-period-face hidden)

(defun calfw--source-period-bgcolor-get (source)
  "Return a background color for period items in SOURCE.

If `calfw-source-period-bgcolor' is nil, the value of
`calfw-source-color' is used."
  (or (calfw-source-period-bgcolor source)
      (let ((c (calfw-make-bg-color
                (calfw-source-color source)
                (calfw-source-period-fgcolor source))))
        (setf (calfw-source-period-bgcolor source) c)
        c)))

(defun calfw--source-period-fgcolor-get (source)
  "Return a foreground color for period items in SOURCE.

If `calfw-source-period-fgcolor' is nil, the black or white
 (negative color of `calfw-source-period-bgcolor') is used."
  (or (calfw-source-period-fgcolor source)
      (let ((c (calfw-make-fg-color
                (calfw-source-color source)
                (calfw-source-period-bgcolor source))))
        (setf (calfw-source-period-fgcolor source) c)
        c)))

(defun calfw-make-fg-color (src-color _bg-color)
  "Return a suitable foreground color for SRC-COLOR.

 Use `calfw-composite-color' with SRC-COLOR, a weight of 0.7, and
 the default face's foreground color."
  ;; The calfw way
  ;; (cl-destructuring-bind
  ;;     (r g b) (color-values (or color "black"))
  ;;   (if (< 147500 (+ r g b)) "black" "white"))
                                        ; (* 65536 3 0.75)
  (calfw-composite-color src-color 0.7 (face-foreground 'default)))

(defun calfw-make-bg-color (src-color _fg-color)
  "Compose SRC-COLOR with the default background color.

Returns a color string."
  (calfw-composite-color src-color 0.3 (face-background 'default)))

(defun calfw-composite-color (clr1 alpha clr2)
  "Return the combination of CLR1 with ALPHA and CLR2.
CLR2 is composited with 1-ALPHA transpancy."
  (let* ((result-rgb (cl-mapcar
                      (lambda (c1 c2)
                        (+ (* alpha c1)
                           (* (- 1 alpha) c2)))
                      (color-name-to-rgb clr1)
                      (color-name-to-rgb clr2))))
    (apply 'color-rgb-to-hex (append result-rgb '(2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calendar event

;; This structure defines calendar events.
(cl-defstruct calfw-event
  title       ; event title [string]
  start-date  ; start date of the event [calfw-date]
  start-time  ; start time of the event (optional)
  end-date    ; end date of the event [calfw-date] (optional)
  end-time    ; end of the event (optional)
  description ; event description [string] (optional)
  location    ; location [string] (optional)
  source      ; [internal] source of the event
  status       ; 'cancelled, 'tentative, 'confirmed or nil
  data        ; reference to event data
  )

(defun calfw-event-overview (event)
  "Extract the event overview string from EVENT."
  (calfw-event-format event calfw-event-format-overview))

(defun calfw-event-days-overview (event)
  "Extract the days overview string from EVENT."
  (calfw-event-format event calfw-event-format-days-overview))

(defun calfw-event-period-overview (event)
  "Extract the period overview string from EVENT."
  (calfw-event-format event calfw-event-format-period-overview))

(defun calfw-event-detail (event)
  "Extract the details string from EVENT."
  (calfw-event-format event calfw-event-format-detail))

(defun calfw--event-format-field-string (string)
  "Return STRING as a format field."
  `((?s . ,string)))

(defun calfw--event-format-field-time (time)
  "Format TIME values for use by `calfw-event-format-field'.
Returns an alist of format characters and formatted time strings."
  `((?H . ,(calfw--event-format-field-number (car time) 2))
    (?M . ,(calfw--event-format-field-number (cadr time) 2))))

(defun calfw--event-format-field-date (date)
  "Format DATE values for use in `calfw-event-format-field'."
  `((?Y . ,(calfw--event-format-field-number (caddr date) 4))
    (?m . ,(calfw--event-format-field-number (car date) 2))
    (?d . ,(calfw--event-format-field-number (cadr date) 2))))

(defun calfw--event-format-field-number (num width)
  "Format NUM as a string of at least WIDTH digits, padded with zeros."
  (format (concat "%0" (number-to-string width) "d") num))

(defun calfw--event-format-field (event field args-fun)
  "Format FIELD of the calfw-event EVENT.

Formatting is done according to the string specified in
`calfw-event-format-FIELD'.  ARGS-FUN is a function to generate
arguments for `format-spec'."
  (let* ((s-name        (symbol-name field))
         (format-string (symbol-value (calfw--sym "calfw-event-format-" s-name)))
         (field-val     (funcall (calfw--sym "calfw-event-" s-name) event)))
    (if field-val
        (format-spec format-string (funcall args-fun field-val))
      "")))

(defun calfw-event-format (event format-string)
  "Format the calfw-event EVENT according to FORMAT-STRING.

The following values are possible:
%t = title, %S = start date, %s = start time, %E = end date,
%e = end time, %l = Location, %d = Description."
  (calfw--tp
   (format-spec
    format-string
    (mapcar #'(lambda (field)
                `(,(car field) . ,(calfw--event-format-field
                                   event (cadr field) (caddr field))))
            '((?t title       calfw--event-format-field-string)
              (?S start-date  calfw--event-format-field-date)
              (?s start-time  calfw--event-format-field-time)
              (?E end-date    calfw--event-format-field-date)
              (?e end-time    calfw--event-format-field-time)
              (?l location    calfw--event-format-field-string)
              (?d description calfw--event-format-field-string))))
   'cfw:source (calfw-event-source event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering Destination

;; This structure object is the abstraction of the rendering
;; destinations, such as buffers, regions and so on.

;; [calfw-dest]
;; type        : identify symbol for destination type. (buffer, region, text)
;; buffer      : a buffer object of rendering destination.
;; min-func    : a function that returns upper limit of rendering destination.
;; max-func    : a function that returns lower limit of rendering destination.
;; width       : width of the reference size.
;; height      : height of the reference size.
;; clear-func  : a function that clears the rendering destination.
;; before-update-func : a function that is called at the beginning of rendering routine.
;; after-update-func  : a function that is called at the end of rendering routine.
;; today-ol    : a list of overlays for today

(cl-defstruct calfw-dest
  type buffer min-func max-func width height
  clear-func before-update-func after-update-func today-ol)

;; shortcut functions
(defmacro calfw-dest-with-region (dest &rest body)
  "Execute BODY in a buffer narrowed to the region specified by DEST.

DEST is a calfw destination.  The buffer is narrowed to the region
specified by `(calfw-dest-point-min DEST)` and
`(calfw-dest-point-max DEST)`."
  (let (($dest (gensym)))
    `(let ((,$dest ,dest))
       (with-current-buffer (calfw-dest-buffer ,$dest)
         (save-restriction
           (narrow-to-region
            (calfw-dest-point-min ,$dest) (calfw-dest-point-max ,$dest))
           ,@body)))))
(put 'calfw-dest-with-region 'lisp-indent-function 1)

(defun calfw-dest-point-min (c)
  "Call the minimum function of calendar calfw-dest C."
  (funcall (calfw-dest-min-func c)))

(defun calfw-dest-point-max (c)
  "Call the maximum function of calendar calfw-dest C."
  (funcall (calfw-dest-max-func c)))

(defun calfw-dest-clear (c)
  "Call the clear function of calfw-dest C."
  (funcall (calfw-dest-clear-func c)))

(defun calfw-dest-before-update (c)
  "Call the before update function of calfw-dest C."
  (when (calfw-dest-before-update-func c)
    (funcall (calfw-dest-before-update-func c))))

(defun calfw-dest-after-update (c)
  "Call the after update function of calfw-dest C."
  (when (calfw-dest-after-update-func c)
    (funcall (calfw-dest-after-update-func c))))

;; private functions

(defun calfw--dest-ol-today-clear (dest)
  "Clear decoration overlays in DEST."
  ;; " Clear decoration overlays."
  (cl-loop for i in (calfw-dest-today-ol dest)
           do (delete-overlay i))
  (setf (calfw-dest-today-ol dest) nil))

(defun calfw--dest-ol-today-set (dest)
  "Highlight today in DEST.

Sets `calfw-dest-today-ol' of DEST to the created overlays."
  ;; " Put a highlight face on today."
  (let (ols)
    (calfw-dest-with-region dest
                            (calfw--find-all-by-date
                             dest (calendar-current-date)
                             (lambda (begin end)
                               (let ((overlay (make-overlay begin end)))
                                 (overlay-put overlay 'face
                                              (if (eq 'calfw-day-title-face
                                                      (get-text-property begin 'face))
                                                  'calfw-today-title-face 'calfw-today-face))
                                 (push overlay ols)))))
    (setf (calfw-dest-today-ol dest) ols)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level API

;; Buffer

(defconst calfw-calendar-buffer-name "*cfw-calendar*" "[internal] Default buffer name for the calendar view.")

(defun calfw-dest-init-buffer (&optional buf width height custom-map)
  "Create a buffer destination.
This destination uses an entire buffer and set up the major-mode
`calfw-calendar-mode' and the key map `calfw-calendar-mode-map'.  BUF
is a buffer name to render the calendar view.  If BUF is nil, the
default buffer name `calfw-calendar-buffer-name' is used.  WIDTH
and HEIGHT are reference size of the calendar view.  If those are
nil, the size of calendar is calculated from the window that
shows BUF or the selected window.  The component
object is stored at the buffer local variable `calfw-component'.
CUSTOM-MAP is the additional keymap that is added to default
keymap `calfw-calendar-mode-map'."
  (let
      ((buffer (or buf (get-buffer-create calfw-calendar-buffer-name)))
       (window (or (and buf (get-buffer-window buf)) (selected-window)))
       dest)
    (setq dest
          (make-calfw-dest
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
      (unless (eq major-mode 'calfw-calendar-mode)
        (calfw-calendar-mode custom-map)))
    dest))

;; Region

(defun calfw-dest-init-region (buf mark-begin mark-end &optional width height)
  "Create a region destination between MARK-BEGIN and MARK-END in BUF.

MARK-BEGIN and MARK-END are markers separated by more than one
character.  Optional WIDTH and HEIGHT specify the width and height
of the region.  This destination is employed to be embedded in
some application buffer.  The destination does not set up any
modes or keymaps for the buffer, and is the responsibility of the
application that uses calfw."
  (let
      ((mark-begin mark-begin) (mark-end mark-end)
       (window (or (get-buffer-window buf) (selected-window))))
    (make-calfw-dest
     :type 'region
     :min-func (lambda () (marker-position mark-begin))
     :max-func (lambda () (marker-position mark-end))
     :buffer buf
     :width (or width (window-width window))
     :height (or height (window-height window))
     :clear-func
     (lambda ()
       (calfw--dest-region-clear (marker-position mark-begin)
                                 (marker-position mark-end))))))

(defun calfw--dest-region-clear (begin end)
  "Delete the content of the region from BEGIN to END."
  (when (< 2 (- end begin))
    (delete-region begin (1- end)))
  (goto-char begin))

;; Inline text

(defconst calfw-dest-background-buffer " *calfw-dest-background*")

(defun calfw-dest-init-inline (width height)
  "Create a text destination with given WIDTH and HEIGHT."
  (let
      ((buffer (get-buffer-create calfw-dest-background-buffer))
       (window (selected-window))
       dest)
    (setq dest
          (make-calfw-dest
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component API

;; Create

(defun calfw--cp-new (dest model view &optional initial-date)
  "Create a new component object for DEST, MODEL, and VIEW.

INITIAL-DATE is the date to display.  VIEW is a symbol specifying
the view type: month, two-weeks, week, or day.  DEST is a
`calfw-dest' object, and MODEL is a model object."
  (let ((cp (make-calfw-component
             :dest  dest
             :model model
             :view  (or view 'month))))
    (calfw--cp-update cp initial-date)
    cp))

;; Getting the component instance

(defun calfw-cp-get-component (&optional noerror)
  "Return the component object on the current cursor position.

If NOERROR is non-nil, return nil if no component is found."
  (or (get-text-property (point) 'cfw:component)
      (if (local-variable-p 'calfw-component (current-buffer))
          (buffer-local-value 'calfw-component (current-buffer))
        (unless noerror
          (error "Not found cfw:component attribute")))))

;; Getter

(defun calfw-cp-get-contents-sources (component &optional exclude-hidden)
  "Return a list of the content sources for COMPONENT.

EXCLUDE-HIDDEN, if non-nil, excludes hidden sources."
  (calfw--model-get-contents-sources (calfw-component-model component)
                                     exclude-hidden))

(defun calfw-cp-get-annotation-sources (component)
  "Return a list of the annotation sources for COMPONENT."
  (calfw--model-get-annotation-sources (calfw-component-model component)))

(defun calfw-cp-get-view (component)
  "Return a symbol of the current view type for COMPONENT."
  (calfw-component-view component))

(defun calfw-cp-get-buffer (component)
  "Return the destination buffer on which the COMPONENT draws the content."
  (calfw-dest-buffer (calfw-component-dest component)))

(defun calfw-cp-displayed-date-p (component date)
  "Return non-nil if DATE is displayed in the current view of COMPONENT."
  (let* ((model (calfw-component-model component))
         (begin (calfw--k 'begin-date model))
         (end (calfw--k 'end-date model)))
    (unless (and begin end) (error "Wrong model : %S" model))
    (calfw-date-between begin end date)))

;; Setter
(defun calfw--cp-move-cursor (dest date &optional force)
  "Move the cursor in DEST to DATE if the cursor is not already on DATE.

FORCE non-nil unconditionally moves the cursor."
  (when (or force
            ;; Check if there's a current component, otherwise
            ;; `calfw-cursor-to-nearest-date' signals an error.
            (null (calfw-cp-get-component t))
            (not (equal (calfw--cursor-to-date) date)))
    (let ((pos (calfw--find-by-date dest date)))
      (when pos
        (goto-char pos)
        (unless (eql (selected-window) (get-buffer-window (current-buffer)))
          (set-window-point (get-buffer-window (current-buffer)) pos))))))

(defun calfw-cp-set-contents-sources (component sources)
  "Set content sources for COMPONENT to SOURCES."
  (calfw--model-set-contents-sources
    sources (calfw-component-model component)))

(defun calfw-cp-set-annotation-sources (component sources)
  "Set annotation sources for COMPONENT to SOURCES."
  (calfw--model-set-annotation-sources
   sources (calfw-component-model component)))

(defun calfw-cp-set-view (component view)
  "Change the view type of COMPONENT and re-draw the content.

VIEW is a symbol of the view type."
  (setf (calfw-component-view component) view)
  (calfw--cp-update component))

(defun calfw-cp-resize (component width height)
  "Resize the COMPONENT size to WIDTH and HEIGHT and re-draw the content."
  (let* ((dest (calfw-component-dest component))
         (buf (calfw-dest-buffer dest))
         (window (or (and buf (get-buffer-window buf)) (selected-window))))
    (setf (calfw-dest-width dest) (or width (window-width window))
          (calfw-dest-height dest) (or height (window-height window)))))

;; Hook

(defun calfw-cp-add-update-hook (component hook)
  "Add HOOK to the update hooks of COMPONENT.

HOOK is a function that has no argument."
  (push hook (calfw-component-update-hooks component)))

(defun calfw-cp-add-click-hook (component hook)
  "Add HOOK to the click hooks of COMPONENT.

HOOK is a function that has no argument."
  (push hook (calfw-component-click-hooks component)))



;;; private methods

(defvar calfw-cp-dipatch-funcs
  '((month             .  calfw--view-month)
    (week              .  calfw--view-week)
    (two-weeks         .  calfw--view-two-weeks)
    (day               .  calfw--view-day))
  "Dispatch functions for calfw views.")

(defun calfw--cp-dispatch-view-impl (view)
  "Return a view function corresponding to the view symbol VIEW."
  (or (alist-get view calfw-cp-dipatch-funcs)
      (error "Not found such view : %s" view)))

(defvar calfw-highlight-today t
  "Variable to control whether today is rendered differently than other days.")

(defun calfw--cp-update (component &optional initial-date)
  "Clear and re-draw the COMPONENT content.

Optional argument INITIAL-DATE specifies the date to display
 after re-drawing."
  (let* ((buf (calfw-cp-get-buffer component))
         (dest (calfw-component-dest component)))
    (with-current-buffer buf
      (calfw-dest-before-update dest)
      (calfw--dest-ol-today-clear dest)
      (let ((buffer-read-only nil))
        (calfw-dest-with-region dest
                                (calfw-dest-clear dest)
                                (funcall (calfw--cp-dispatch-view-impl
                                          (calfw-component-view component))
                                         component)))
      (when calfw-highlight-today
        (calfw--dest-ol-today-set dest))
      (when initial-date
        (calfw-cp-goto-date component initial-date))
      (calfw-dest-after-update dest)
      (calfw--cp-fire-update-hooks component))))

(defun calfw--cp-fire-click-hooks (component)
  "Call click hook functions of COMPONENT with no arguments."
  (cl-loop for f in (calfw-component-click-hooks component)
           do (condition-case err
                  (funcall f)
                (error (message "Calfw: Click / Hook error %S [%s]" f err)))))

(defun calfw--cp-fire-update-hooks (component)
  "Call update hook functions of the COMPONENT with no arguments."
  (cl-loop for f in (calfw-component-update-hooks component)
           do (condition-case err
                  (funcall f)
                (error (message "Calfw: Update / Hook error %S [%s]" f err)))))



;;; Models

(defvar calfw-default-text-sorter 'string-lessp "[internal] Default sorting criteria in a calendar cell.")

(defun calfw-model-abstract-new (date contents-sources annotation-sources &optional sorter)
  "Return an abstract model object.

DATE is the initial date, CONTENTS-SOURCES is a list of contents
functions, ANNOTATION-SOURCES is a list of annotation functions, and
SORTER is a function to sort the contents."
  (unless date (setq date (calendar-current-date)))
  `((init-date . ,date)
    (contents-sources . ,contents-sources)
    (annotation-sources . ,annotation-sources)
    (sorter . ,(or sorter calfw-default-text-sorter))))

(defun calfw-model-abstract-derived (date org-model)
  "Return an abstract model object.

The contents functions and annotation ones are copied from ORG-MODEL.
DATE is initial date for the calculation of the start date and end one.
ORG-MODEL is a model object to inherit."
  (calfw-model-abstract-new
   date
   (calfw--model-get-contents-sources org-model)
   (calfw--model-get-annotation-sources org-model)
   (calfw-model-get-sorter org-model)))

(defun calfw--model-create-updated-view-data (model view-data)
  "Clear previous view model data from MODEL and return a new model.

The new model is created with VIEW-DATA."
  (append
   (calfw-model-abstract-derived
    (calfw--k 'init-date model) model)
   view-data))

;; public functions

(defun calfw-model-get-holiday-by-date (date model)
  "Return a holiday title on the DATE in MODEL."
  (calfw--contents-get date (calfw--k 'holidays model)))

(defun calfw-model-get-contents-by-date (date model)
  "Return a list of contents on the DATE in MODEL."
  (calfw--contents-get date (calfw--k 'contents model)))

(defun calfw-model-get-annotation-by-date (date model)
  "Return an annotation on the DATE in MODEL."
  (calfw--contents-get date (calfw--k 'annotations model)))

(defun calfw-model-get-periods-by-date (date model)
  "Return a list of periods on the DATE in the MODEL."
  (cl-loop for (begin end event) in (calfw--k 'periods model)
           for content = (if (calfw-event-p event)
                             (calfw-event-detail event)
                           event)
           if (calfw-date-between begin end date)
           collect `(,begin ,end ,content)))

(defun calfw-model-get-sorter (model)
  "Return a sorter function from the calendar MODEL."
  (calfw--k 'sorter model))

;; private functions

(defun calfw--model-get-contents-sources (model &optional exclude-hidden)
  "Return a list of content sources of the MODEL.

If EXCLUDE-HIDDEN is non-nil, exclude hidden sources."
  (let ((sources (calfw--k 'contents-sources model)))
    (if exclude-hidden
        (seq-filter (lambda (s) (not (calfw-source-hidden s)))
                    sources)
      sources)))

(defun calfw--model-get-annotation-sources (model)
  "Return a list of annotation sources of the MODEL."
  (calfw--k 'annotation-sources model))

(defun calfw--model-set-init-date (date model)
  "Set the DATE that is used to calculate the display period of MODEL.

Returns DATE."
  (let ((cell (assq 'init-date model)))
    (cond
     (cell (setcdr cell date))
     (t (push (cons 'init-date date) model))))
  date)

(defun calfw--model-set-contents-sources (sources model)
  "Set the content SOURCES of the MODEL.

Return SOURCES."
  (let ((cell (assq 'contents-sources model)))
    (cond
     (cell (setcdr cell sources))
     (t (push (cons 'contents-sources sources) model))))
  sources)

(defun calfw--model-set-annotation-sources (sources model)
  "Set the annotation SOURCES of MODEL.

Returns SOURCES."
  (let ((cell (assq 'annotation-sources model)))
    (cond
     (cell (setcdr cell sources))
     (t (push (cons 'annotation-sources sources) model))))
  sources)

(defun calfw--contents-get (date contents)
  "Return a list of contents on the DATE from CONTENTS."
  (cdr (calfw--contents-get-internal date contents)))

(defun calfw--contents-get-internal (date contents)
  "Return a cons cell that has the key DATE in CONTENTS.

One can modify the returned cons cell destructively.
Returns nil if DATE is not found in CONTENTS."
  (cond
   ((or (null date) (null contents)) nil)
   (t (cl-loop for i in contents
               if (equal date (car i))
               return i
               finally return nil))))

(defun calfw--contents-add (date content contents)
  "Add a record, DATE as a key and CONTENT as a body, to CONTENTS destructively.
Returns the modified contents list.

If CONTENTS has a record for DATE, this function appends CONTENT to the
record."
  (let* ((prv (calfw--contents-get-internal date contents))
         (lst (if (listp content) (copy-sequence content) (list content))))
    (if prv
        (setcdr prv (append (cdr prv) lst))
      (push (cons date lst) contents)))
  contents)

(defun calfw--contents-merge (begin end sources)
  "Return a contents alist between BEGIN date and END, using SOURCES."
  (cond
   ((null sources) nil)
   (t
    (cl-loop for s in sources
             for f = (calfw-source-data s)
             for cnts = (calfw--contents-put-source
                         (funcall f begin end) s)
             with contents = nil
             do
             (cl-loop for c in cnts
                      for (d . line) = c
                      do (setq contents (calfw--contents-add d line contents)))
             finally return contents))))

(defun calfw-periods-put-source (periods source)
  "Associate SOURCE with each period in PERIODS.

Returns a list of the form \\='((START-DATE END-DATE EVENT) ...)."
  (cl-loop for period in periods
           collect
           (cond
            ((calfw-event-p period)
             (setf (calfw-event-source period) source)
             `(,(calfw-event-start-date period)
               ,(calfw-event-end-date period)
               ,period))
            (t
             (cl-destructuring-bind (begin end . summaries) period
               (list begin end
                     (calfw--tp (if (listp summaries)
                                    (mapconcat 'identity (calfw-flatten summaries) " ")
                                  summaries)
                                'cfw:source source)))))))

(defun calfw--contents-put-source (contents source)
  "Put the SOURCE object in the `calfw-source' text property in CONTENTS.

During rendering, the SOURCE object is used to put some face
property."
  (cond
   ((null source) contents)
   (t
    (cl-loop for content in contents
             collect
             (cond
              ((calfw-event-p content)
               (setf (calfw-event-source content) source)
               `(,(calfw-event-start-date content) ,content))
              ((eq (car content) 'periods)
               (cons 'periods
                     (calfw-periods-put-source (cdr content) source)))
              (t
               (cons (car content)
                     (cl-loop for i in (cdr content)
                              collect (calfw--tp i 'cfw:source source)))))))))

(defun calfw--annotations-merge (begin end sources)
  "Return an annotation alist between BEGIN date and END date.

Call functions `calfw-annotations-functions' from SOURCES."
  (cond
   ((null sources) nil)
   ((= 1 (length sources))
    (funcall (calfw-source-data (car sources)) begin end))
   (t
    (cl-loop for s in sources
             for f = (calfw-source-data s)
             for cnts = (funcall f begin end)
             with annotations = nil
             do
             (cl-loop for c in cnts
                      for (d . line) = c
                      for prv = (calfw--contents-get-internal d annotations)
                      if prv
                      do (setcdr prv (concat (cdr prv) "/" line))
                      else
                      do (push (cons d line) annotations))
             finally return annotations))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering Utilities

(defun calfw-render-title-month (date)
  "Render the calendar title for the monthly view, given DATE."
  (format "%4s / %s"
          (calendar-extract-year date)
          (aref calendar-month-name-array
                (1- (calendar-extract-month date)))))

(defun calfw-render-title-period (begin-date end-date)
  "Render the calendar title for the period view between BEGIN-DATE and END-DATE."
  (cond
   ((eql (calendar-extract-month begin-date) (calendar-extract-month end-date))
    (format "%4s / %s %s - %s"
            (calendar-extract-year begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month begin-date)))
            (calendar-extract-day begin-date)
            (calendar-extract-day end-date)))
   (t
    (format "%4s / %s %s - %s %s"
            (calendar-extract-year begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month begin-date)))
            (calendar-extract-day begin-date)
            (aref calendar-month-name-array (1- (calendar-extract-month end-date)))
            (calendar-extract-day end-date)))))

(defun calfw-render-title-day (date)
  "Render the calendar title for the day view on DATE."
  (format "%4s / %s %s"
          (calendar-extract-year date)
          (aref calendar-month-name-array
                (1- (calendar-extract-month date)))
          (calendar-extract-day date)))

(defun calfw--render-center (width string &optional padding)
  "Format STRING in the center, padding to WIDTH with PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (calfw--render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (/ (- width len) 2)))
    (concat
     (make-string margin padding) cnt
     (make-string (- width len margin) padding))))

(defun calfw--render-left (width string &optional padding)
  "Format STRING, padding on the right with the character PADDING to WIDTH."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (calfw--render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat cnt (make-string margin padding))))

(defun calfw--render-separator (string)
  "Add a separator into the ROWS list, using STRING.
Returns STRING."
  ;; " Add a separator into the ROWS list."
  (when (get-text-property 0 'cfw:item-separator string)
    (let ((last-face (get-text-property 0 'face string)))
      (cond
       ((or (null last-face) (listp last-face))
        (setq last-face (append last-face `(:underline ,calfw-item-separator-color-face)))
        (put-text-property 0 (length string) 'face last-face string)
        (put-text-property 0 (length string) 'font-lock-face last-face string))
       ((symbolp last-face)
        (let ((attrs (face-all-attributes last-face (selected-frame))))
          (setq attrs ; transform alist to plist
                (cl-loop with nattrs = nil
                         for (n . v) in (append attrs `((:underline . ,calfw-item-separator-color-face)))
                         do (setq nattrs (cons n (cons v nattrs)))
                         finally return nattrs))
          (put-text-property 0 (length string) 'face attrs string)
          (put-text-property 0 (length string) 'font-lock-face attrs string)))
       (t
        (message "DEBUG? CALFW- FACE %S / %S" string last-face)))))
  string)

(defun calfw--render-right (width string &optional padding)
  "Format STRING of WIDTH, padding on the left with the character PADDING."
  ;; " Format STRING, padding on the left with the character PADDING."
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string
                       (calfw--render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat (make-string margin padding) cnt)))

(defun calfw--render-add-right (width left right &optional padding)
  "Layout strings LEFT and RIGHT within WIDTH.

LEFT and RIGHT are truncated to fit within WIDTH, adding PADDING as
appropriate."
  (let* ((padding (or padding ?\ ))
         (lcnt (or (and left
                        (calfw--render-truncate left width t))
                   ""))
         (llen (string-width lcnt))
         (rmargin (- width llen))
         (right (string-trim right))
         (rcnt (or (and right (> rmargin 0)
                        (calfw--render-truncate right rmargin))
                   ""))
         (cmargin (- width llen (string-width rcnt))))
    (concat lcnt (if (< 0 cmargin) (make-string cmargin padding)) rcnt)))

(defun calfw--render-sort-contents (lst sorter)
  "Sort the string list LST using SORTER.
Returns the sorted list."
  (sort (copy-sequence lst) sorter))

(defun calfw--render-get-face-period (text default-face)
  "Return a face for the source object of the period TEXT.

The face is derived from the `cfw:source' text property of TEXT,
and DEFAULT-FACE is returned if the source or background color
are nil."
  (let* ((src (get-text-property 0 'cfw:source text))
         (bg-color (and src (calfw--source-period-bgcolor-get src)))
         (fg-color (and src (calfw--source-period-fgcolor-get src))))
    (cond
     ((or (null src) (null bg-color)) default-face)
     (t (append (list ':background bg-color ':foreground fg-color)
                (calfw-source-opt-period-face src))))))

(defun calfw--render-get-face-content (text default-face)
  "Return a face for the source object of the content TEXT.

The face is derived from the `cfw:source' text property of TEXT,
or DEFAULT-FACE if the source or its foreground color is nil."
  (let* ((src (get-text-property 0 'cfw:source text))
         (fg-color (and src (calfw-source-color src))))
    (cond
     ((or (null src) (null fg-color)) default-face)
     (t (append (list ':foreground (calfw-make-fg-color fg-color fg-color)
                      ':background (calfw-make-bg-color fg-color fg-color))
                (calfw-source-opt-face src))))))

(defun calfw--render-default-content-face (str &optional default-face)
  "Put the default content face on STR, retaining existing faces.

Use DEFAULT-FACE if non-nil.  Return the modified string."
  ;;
  (cl-loop for i from 0 below (length str)
           with ret = (substring str 0)
           with face = (or default-face
                           (calfw--render-get-face-content
                            str 'calfw-default-content-face))
           unless (get-text-property i 'face ret)
           do
           (put-text-property i (1+ i) 'face face ret)
           (put-text-property i (1+ i) 'font-lock-face face ret)
           finally return ret))

(defun calfw--render-get-week-face (daynum &optional default-face)
  "Return face for DAYNUM, or DEFAULT-FACE if DAYNUM is not a weekend."
  ;;
  (cond
   ((= daynum calfw-week-saturday)
    'calfw-saturday-face)
   ((= daynum calfw-week-sunday)
    'calfw-sunday-face)
   (t default-face)))

(defun calfw--render-truncate (org limit-width &optional ellipsis)
  "Truncate a string ORG to LIMIT-WIDTH, like `truncate-string-to-width'.

ELLIPSIS is the string to use as ellipsis."
  ;; "
  (setq org (replace-regexp-in-string "\n" " " org))
  (if (< limit-width (string-width org))
      (let ((str (truncate-string-to-width
                  (substring org 0) limit-width 0 nil ellipsis)))
        (unless (get-text-property 0 'help-echo str)
          (calfw--tp str 'help-echo org))
        str)
    org))

(defface calfw-toolbar-face
  '((((class color) (background light))
     :foreground "Gray90" :background "Gray90")
    (((class color) (background dark))
     :foreground "Steelblue4" :background "Steelblue4"))
  "Face for toolbar."
  :group 'calfw)

(defface calfw-toolbar-button-off-face
  '((((class color) (background light))
     :foreground "Lightskyblue4" :background "White")
    (((class color) (background dark))
     :foreground "Gray10" :weight bold :background "Steelblue4"))
  "Face for button on toolbar."
  :group 'calfw)

(defface calfw-toolbar-button-on-face
  '((((class color) (background light))
     :foreground "Lightpink3" :background "Gray94" )
    (((class color) (background dark))
     :foreground "Gray50" :weight bold :background "Steelblue4"))
  "Face for button on toolbar."
  :group 'calfw)

(defun calfw--render-button (title command &optional state)
  "Return a decorated text for the toolbar buttons.

TITLE is a button title, COMMAND is an interactive command called by
clicking.  If STATE is non-nil, the face `calfw-toolbar-button-on-face'
is applied, otherwise `calfw-toolbar-button-off-face' is applied."
  (let ((text (concat "[" title "]"))
        (keymap (make-sparse-keymap)))
    (calfw--rt text (if state 'calfw-toolbar-button-on-face
                      'calfw-toolbar-button-off-face))
    (define-key keymap [mouse-1] command)
    (calfw--tp text 'keymap keymap)
    (calfw--tp text 'mouse-face 'highlight)
    text))

(defvar calfw-toolbar-buttons
  '((("Today" . calfw-navi-goto-today-command))
    .
    (("Day" . (:view day))
     ("Week" . (:view week))
     ("Two Weeks" . (:view two-weeks))
     ("Month" . (:view month))))
  "Buttons to be rendered in toolbar.

The car are buttons on the left (after left/right), the cdr are
buttons on the right. Each button description is a cons (TITLE .
FN) where TITLE is the button title and FN is the function to
call. If FN is a (:view VIEW) then it is pressing the button
changes the view to VIEW.")

(defun calfw--render-toolbar (width current-view prev-cmd next-cmd)
  "Return a text string of the toolbar.

WIDTH is the width of the toolbar.  CURRENT-VIEW is a symbol
representing the current view type.  PREV-CMD and NEXT-CMD are
the commands for moving the view."
  (let* ((prev (calfw--render-button " < " prev-cmd))
         (next (calfw--render-button " > " next-cmd))
         (sp  " "))
    (cl-labels
        ((format-btns (btns)
           (cl-loop for (title . fn) in btns
                    concat
                    (if (eq (car-safe fn) :view)
                        (let ((view (cadr fn)))
                          (calfw--render-button
                           title
                           (lambda ()
                             (interactive)
                             (calfw-cp-set-view (calfw-cp-get-component)
                                                view))
                           (eq current-view view)))
                      (calfw--render-button title fn))
                    concat sp)))
      (calfw--render-default-content-face
       (calfw--render-add-right
        width
        (concat
         sp prev sp next sp
         (format-btns (car calfw-toolbar-buttons)))
        (format-btns (cdr calfw-toolbar-buttons)))
       'calfw-toolbar-face))))

(defun calfw-event-mouse-click-toggle-calendar (event)
  "Toggle the `calfw-source-hidden' property of calendar source at EVENT."
  (interactive "e")
  (when-let ((s (get-text-property
                 (posn-point (event-start event))
                 'cfw:source)))
    (setf (calfw-source-hidden s)
          (not (calfw-source-hidden s)))
    (calfw--cp-update (calfw-cp-get-component))))

(defun calfw-event-toggle-calendar (source)
  "Toggle visibility of calendar SOURCE."
  (interactive (list
                (get-text-property (point) 'cfw:source)))
  (when source
    (if current-prefix-arg
        (let* ((comp (calfw-cp-get-component))
               (sources (calfw--model-get-contents-sources
                         (calfw-component-model comp))))
          (dolist (src sources)
            (unless (eq src source)
              (setf (calfw-source-hidden src)
                    (not (calfw-source-hidden src))))))
      (setf (calfw-source-hidden source)
            (not (calfw-source-hidden source))))
    (calfw--cp-update (calfw-cp-get-component))))

(defun calfw-event-toggle-all-calendars ()
  "Show all calendars in the current view.
If all calendars are already shown, hide them all."
  (interactive)
  (when (calfw-cp-get-component)
    (let* ((comp (calfw-cp-get-component))
           (sources (calfw--model-get-contents-sources
                     (calfw-component-model comp)))
           (all-shown (not (cl-some
                            'identity
                            (cl-loop for s in sources
                                     collect
                                     (calfw-source-hidden s))))))
      (cl-loop for s in sources do
               (setf (calfw-source-hidden s)
                     all-shown))
      (calfw--cp-update comp))))

(defun calfw--render-footer (_width sources)
  "Return a text of the footer.

The footer is rendered based on the SOURCES."
  (let* ((spaces (make-string 5 ? ))
         (whole-text
          (mapconcat
           'identity
           (cl-loop
            with keymap = (progn
                            (let ((kmap (make-sparse-keymap)))
                              (define-key kmap [mouse-1] 'calfw-event-mouse-click-toggle-calendar)
                              (define-key kmap [13] 'calfw-event-toggle-calendar)
                              kmap))
            for s in sources
            for hidden-p = (calfw-source-hidden s)
            for title = (calfw--tp (substring (calfw-source-name s) 0)
                                   'cfw:source s)
            for dot   = (calfw--tp (substring "(==)" 0) 'cfw:source s)
            collect
            (progn
              (calfw--tp dot 'mouse-face 'highlight)
              (propertize
               (calfw--render-default-content-face
                (concat
                 "[" (calfw--rt dot
                                (if hidden-p
                                    'calfw-calendar-hidden-face
                                  (calfw--render-get-face-period dot 'calfw-periods-face)))
                 " " title "]")
                (if hidden-p
                    'calfw-calendar-hidden-face
                  (calfw--render-get-face-content title
                                                  'calfw-default-content-face)))
               'keymap keymap)))
           (concat "\n" spaces))))
    (concat
     spaces
     whole-text)))

(defun calfw--render-periods (date week-day periods-stack cell-width)
  "Translate PERIODS-STACK to display content on DATE.

 WEEK-DAY and CELL-WIDTH are used to render the periods title."
  (cl-loop with prev-row = -1
           for (row (begin end content props)) in (sort periods-stack
                                                        (lambda (a b)
                                                          (< (car a) (car b))))
           nconc (make-list (- row prev-row 1) "") ; add empty padding lines
           do (setq prev-row row)

           for beginp = (equal date begin)
           for endp   = (equal date end)
           for inwidth  = (- cell-width (if beginp 1 0) (if endp 1 0))
           for title  = (calfw--render-periods-title
                         date week-day begin end content cell-width inwidth)
           collect
           (apply 'propertize
                  (concat (when beginp calfw-fstring-period-start)
                          (calfw--render-left inwidth title ?-)
                          (when endp calfw-fstring-period-end))
                  'face (calfw--render-get-face-period content 'calfw-periods-face)
                  'font-lock-face (calfw--render-get-face-period content 'calfw-periods-face)
                  'cfw:period t
                  props)))

(defun calfw--render-periods-title (date week-day begin end content cell-width inwidth)
  "Return a title string for DATE.

Return nil if CONTENT is nil.  WEEK-DAY, BEGIN, END,
CELL-WIDTH, and INWIDTH are also arguments."
  (let* ((week-begin (calfw-date-after date (- week-day)))
         ;; (month-begin (calfw-date
         ;;               (calendar-extract-month date)
         ;;               1 (calendar-extract-year date)))
         (title-begin-abs
          (max
           (calendar-absolute-from-gregorian begin)
           (calendar-absolute-from-gregorian week-begin)))
         ;; (title-begin (calendar-gregorian-from-absolute title-begin-abs))
         (num (- (calendar-absolute-from-gregorian date) title-begin-abs)))
    (when content
      (cl-loop with title = (substring content 0)
               for i from 0 below num
               for pdate = (calendar-gregorian-from-absolute (+ title-begin-abs i))
               for chopn = (+ (if (equal begin pdate) 1 0) (if (equal end pdate) 1 0))
               for del = (truncate-string-to-width title (- cell-width chopn))
               do
               (setq title (substring title (length del)))
               finally return
               (calfw--render-truncate title inwidth (equal end date))))))

;; event periods shifts pos - not one line
(defun calfw--render-periods-get-min (periods-each-days begin end)
  "Find the minimum empty row number of the days between BEGIN and END.

PERIODS-EACH-DAYS contains the periods."
  (cl-loop for row-num from 0 below 30 ; assuming the number of stacked periods is less than 30
           unless
           (cl-loop for d in (calfw-enumerate-days begin end)
                    for periods-stack = (calfw--contents-get d periods-each-days)
                    if (and periods-stack (assq row-num periods-stack))
                    return t)
           return row-num))

(defun calfw--render-periods-place (periods-each-days row period)
  "Assign PERIOD content to the ROW'th row on the days of the period.

Return PERIODS-EACH-DAYS."
  (cl-loop for d in (calfw-enumerate-days (car period) (cadr period))
           for periods-stack = (calfw--contents-get-internal d periods-each-days)
           if periods-stack
           do (setcdr periods-stack (append (cdr periods-stack)
                                            (list (list row period))))
           else
           do (push (cons d (list (list row period))) periods-each-days))
  periods-each-days)

(defun calfw--render-periods-stacks (model)
  "Arrange the `periods' records of the MODEL and create period-stacks.

period-stack -> ((row-num . period) ... )"
  (let* (periods-each-days)
    (cl-loop for (begin end event) in (calfw--k 'periods model)
             for content = (if (calfw-event-p event)
                               (calfw-event-period-overview event)
                             event)
             for period = (list begin end content
                                (calfw--extract-text-props content 'face))
             for row = (calfw--render-periods-get-min periods-each-days begin end)
             do
             (setq periods-each-days (calfw--render-periods-place
                                      periods-each-days row period)))
    periods-each-days))

(defun calfw--render-columns (day-columns param)
  "Concatenate each row on the days in DAY-COLUMNS into a string of a line.

DAY-COLUMNS is a list of columns.  A column is a list of the form
\\(DATE \\(DAY-TITLE . ANNOTATION-TITLE) STRING STRING...).  PARAM is a
plist of parameters."
  (let ((cell-width  (calfw--k 'cell-width  param))
        (cell-height (calfw--k 'cell-height param))
        (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
        ;; (hline (calfw-k 'hline param))
        (cline (calfw--k 'cline param)))
    ;; day title
    (cl-loop for day-rows in day-columns
             for date = (car day-rows)
             for (tday . ant) = (cadr day-rows)
             do
             (insert
              VL (if date
                     (calfw--tp
                      (calfw--render-default-content-face
                       (calfw--render-add-right cell-width tday ant)
                       'calfw-day-title-face)
                      'cfw:date date)
                   (calfw--render-left cell-width ""))))
    (insert VL EOL)
    ;; day contents
    (cl-loop with breaked-day-columns =
             (cl-loop for day-rows in day-columns
                      for (date _ants . lines) = day-rows
                      collect
                      (cons date (calfw--render-break-lines
                                  lines cell-width (1- cell-height))))
             for i from 1 below cell-height do
             (cl-loop for day-rows in breaked-day-columns
                      for date = (car day-rows)
                      for row = (nth i day-rows)
                      do
                      (insert
                       VL (calfw--tp
                           (calfw--render-separator
                            (calfw--render-left cell-width (and row (format "%s" row))))
                           'cfw:date date)))
             (insert VL EOL))
    (insert cline)))

(defvar calfw-render-line-breaker 'calfw-render-line-breaker-simple
  "A function which breaks a long line into some lines.

The function takes STRING, LINE-WIDTH and MAX-LINE-NUMBER as
arguments.  Calfw has 3 strategies: none, simple and wordwrap.
`calfw-render-line-breaker-none' never breaks lines.
`calfw-render-line-breaker-simple' breaks lines with rigid width
\(default).  `calfw-render-line-breaker-wordwrap' breaks lines
with the Emacs function `fill-region'.")

(defun calfw--render-break-lines (lines cell-width cell-height)
  "Return LINES split into multiple lines based on CELL-WIDTH and CELL-HEIGHT.

Uses `calfw-render-line-breaker'."
  (and lines
       (let ((num (/ cell-height (length lines))))
         (cond
          ((> 2 num) lines)
          (t
           (cl-loop with total-rows = nil
                    for line in lines
                    for rows = (funcall calfw-render-line-breaker line cell-width num)
                    do
                    (when total-rows
                      (calfw--render-add-item-separator-sign total-rows))
                    (setq total-rows (append total-rows rows))
                    finally return total-rows))))))

(defun calfw--render-add-item-separator-sign (rows)
  "Add a separator into the ROWS list.

Adds a `cfw:item-separator' text property to the last line of the
ROWS list, unless it already has a `cfw:period' property.  Returns
ROWS."
  (let ((last-line (car (last rows))))
    (unless (get-text-property 0 'cfw:period last-line)
      (put-text-property 0 (length last-line) 'cfw:item-separator t last-line))
    rows))

(defun calfw-render-line-breaker-none (line _w _n)
  "Return LINE in a list."
  (list line))

(defun calfw-render-line-breaker-simple (string line-width max-line-num)
  "Split STRING into lines of width LINE-WIDTH, with at most MAX-LINE-NUM lines.

Return a list of strings."
  (cl-loop with ret = nil    with linenum = 1
           with curcol = 0   with lastpos = 0
           with endpos = (1- (length string))
           for i from 0 upto endpos
           for c = (aref string i)
           for w = (char-width c)
           for wsum = (+ curcol w) do
           (cond
            ((and (< i endpos) (<= max-line-num linenum))
             (push (string-trim
                    (replace-regexp-in-string
                     "[\n\r]" " " (substring string lastpos))) ret)
             (setq i endpos))
            ((= endpos i)
             (push (substring string lastpos) ret))
            ((or (= c 13) (= c 10))
             (push (substring string lastpos i) ret)
             (setq lastpos (1+ i) curcol 0)
             (cl-incf linenum))
            ((= line-width wsum)
             (push (substring string lastpos (1+ i)) ret)
             (setq lastpos (1+ i) curcol 0)
             (cl-incf linenum))
            ((< line-width wsum)
             (push (substring string lastpos i) ret)
             (setq lastpos i curcol w)
             (cl-incf linenum))
            (t (cl-incf curcol w)))
           finally return (or (and ret (nreverse ret)) '(""))))

(defun calfw-render-line-breaker-wordwrap (string line-width max-line-num)
  "Break STRING into a list of strings, each no wider than LINE-WIDTH.

Uses `fill-region' for word wrapping.  Limits the number of lines
to MAX-LINE-NUM.  Returns a list of strings."
  (if (<= (length string) line-width)
      (list string)
    (let ((fill-column line-width) (use-hard-newlines t))
      (with-temp-buffer
        (insert string)
        (fill-region (point-min) (point-max))
        ;; collect lines
        (goto-char (point-min))
        (let ((cont t) (last (point)) ps ret)
          (while cont
            (setq ps (re-search-forward "\n" nil t))
            (cond
             ((null ps) (setq cont nil)
              (when (not (eobp))
                (push (buffer-substring last (point-max)) ret)))
             (t
              (push (string-trim (buffer-substring last (1- ps))) ret)
              (when (<= max-line-num (length ret))
                (setq cont nil))
              (setq last ps))))
          (or (and ret (nreverse ret)) '("")))))))

(defun calfw--render-append-parts (param)
  "Append rendering parts to PARAM and return a new list."
  (let* ((EOL "\n")
         (cell-width (calfw--k 'cell-width param))
         (columns (calfw--k 'columns param))
         (num-cell-char
          (/ cell-width (char-width calfw-fchar-horizontal-line))))
    (append
     param
     `((eol . ,EOL) (vl . ,(calfw--rt (make-string 1 calfw-fchar-vertical-line) 'calfw-grid-face))
       (hline . ,(calfw--rt
                  (concat
                   (cl-loop for i from 0 below columns concat
                            (concat
                             (make-string 1 (if (= i 0) calfw-fchar-top-left-corner calfw-fchar-top-junction))
                             (make-string num-cell-char calfw-fchar-horizontal-line)))
                   (make-string 1 calfw-fchar-top-right-corner) EOL)
                  'calfw-grid-face))
       (cline . ,(calfw--rt
                  (concat
                   (cl-loop for i from 0 below columns concat
                            (concat
                             (make-string 1 (if (= i 0) calfw-fchar-left-junction calfw-fchar-junction))
                             (make-string num-cell-char calfw-fchar-horizontal-line)))
                   (make-string 1 calfw-fchar-right-junction) EOL) 'calfw-grid-face))))))

(defun calfw--render-day-of-week-names (model param)
  "Insert week names based on MODEL.

Iterates through the headers in MODEL, inserting the day names
using `calendar-day-name-array'.  PARAM specifies cell width and
vertical line."
  (cl-loop for i in (calfw--k 'headers model)
           with VL = (calfw--k 'vl param) with cell-width = (calfw--k 'cell-width param)
           for name = (aref calendar-day-name-array i) do
           (insert VL (calfw--rt (calfw--render-center cell-width name)
                                 (calfw--render-get-week-face i 'calfw-header-face)))))

(defun calfw--render-calendar-cells-weeks (model param title-func)
  "Insert calendar cells for week based views, using MODEL.

Iterates over the weeks in `(calfw--k \\='weeks MODEL)' and calls
`calfw--render-calendar-cells-days' for each week.  PARAM and
TITLE-FUNC are passed to `calfw--render-calendar-cells-days'."
  (cl-loop for week in (calfw--k 'weeks model) do
           (calfw--render-calendar-cells-days model param title-func week
                                              'calfw--render-event-overview-content
                                              t)))

(defun calfw--render-rows-prop (rows)
  "Put a marker as a text property for TAB navigation in ROWS.
Returns the ROWS with text properties added."
  (cl-loop with i = 0
           for line in rows
           collect
           (prog1
               (calfw--tp line 'cfw:row-count i)
             (if (< 0 (length line)) (cl-incf i)))))

(defun calfw--render-map-event-content (lst event-fun)
  "Map EVENT-FUN over LST, applying it to `calfw-event's."
  (mapcar #'(lambda (evt)
              (if (calfw-event-p evt)
                  (funcall event-fun evt)
                evt))
          lst))

(defun calfw--render-event-overview-content (lst)
  "Apply `calfw-event-overview' on `calfw-event's in LST."
  (calfw--render-map-event-content lst 'calfw-event-overview))

(defun calfw--render-event-days-overview-content (lst)
  "Apply `calfw-event-days-overview' on `calfw-event's in LST."
  (calfw--render-map-event-content lst 'calfw-event-days-overview))

(defun calfw--render-event-details-content (lst)
  "Apply `calfw-event-detail' on `calfw-event's in LST."
  (calfw--render-map-event-content lst 'calfw-event-detail))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Views

;;; view model utilities

(defun calfw--view-model-make-weeks (begin-date end-date)
  "Return a list of weeks (list of dates) between BEGIN-DATE and END-DATE."
  (let* (;; (first-day-day (calendar-day-of-week begin-date))
         weeks)
    (cl-loop with i = begin-date
             with day = calendar-week-start-day
             with week = nil
             do
             ;; flush a week
             (when (and (= day calendar-week-start-day) week)
               (push (nreverse week) weeks)
               (setq week nil)
               (when (calfw-date-less-equal-p end-date i) (cl-return)))
             ;; add a day
             (push i week)
             ;; increment
             (setq day (% (1+ day) calfw-week-days))
             (setq i (calfw-date-after i 1)))
    (nreverse weeks)))

(defun calfw--view-model-make-days (begin-date end-date)
  "Return a list of days from BEGIN-DATE to END-DATE, inclusive."
  (cl-loop with days = nil
           with i = begin-date
           do
           (push i days)
           (when (calfw-date-less-equal-p end-date i)
             (cl-return (reverse days)))
           (setq i (calfw-date-after i 1))))

(defun calfw--view-model-make-day-names-for-week ()
  "Return a list of indices representing the days of the week."
  (cl-loop for i from 0 below calfw-week-days
           collect (% (+ calendar-week-start-day i) calfw-week-days)))

(defun calfw--view-model-make-day-names-for-days (begin-date end-date)
  "Return a list of the days of the week between BEGIN-DATE and END-DATE."
  (cl-loop with day = (calendar-day-of-week begin-date)
           with day-names = nil
           with i = begin-date
           do
           (push day day-names)
           (when (calfw-date-less-equal-p end-date i)
             (cl-return (reverse day-names)))
           (setq day (% (1+ day) calfw-week-days))
           (setq i (calfw-date-after i 1))))

(defvar displayed-month) ; because these variables are binded dynamically.
(defvar displayed-year)

(defun calfw--view-model-make-holidays (date)
  "Return an alist of holidays around DATE."
  (if calfw-display-calendar-holidays
      (let ((displayed-month (calendar-extract-month date))
            (displayed-year (calendar-extract-year date)))
        (calendar-holiday-list))))

(defun calfw--view-model-make-common-data (model begin-date end-date &optional lst)
  "Return an alist of common data for MODEL between BEGIN-DATE and END-DATE.

Return value is appended to LST if provided."
  (let* ((contents-all (calfw--contents-merge
                        begin-date end-date
                        (calfw--model-get-contents-sources model t))))
    (append
     `(; common data
       (begin-date . ,begin-date) (end-date . ,end-date)
       (holidays . ,(calfw--view-model-make-holidays begin-date)) ; an alist of holidays, (DATE HOLIDAY-NAME)
       (annotations . ,(calfw--annotations-merge ; an alist of annotations, (DATE ANNOTATION)
                        begin-date end-date
                        (calfw--model-get-annotation-sources model)))
       (contents . ,(cl-loop for i in contents-all
                             unless (eq 'periods (car i))
                             collect i)) ; an alist of contents, (DATE LIST-OF-CONTENTS)
       (periods . ,(calfw--k 'periods contents-all))) ; a list of periods, (BEGIN-DATE END-DATE SUMMARY)
     lst)))

(defun calfw--view-model-make-common-data-for-weeks (model begin-date end-date)
  "Return a model object for week based views from MODEL, BEGIN-DATE and END-DATE."
  (calfw--model-create-updated-view-data
   model
   (calfw--view-model-make-common-data
    model begin-date end-date
    `((headers . ,(calfw--view-model-make-day-names-for-week)) ; a list of the index of day-of-week
      (weeks . ,(calfw--view-model-make-weeks ; a matrix of day-of-month, which corresponds to the index of `headers'
                 (calfw-week-begin-date begin-date)
                 (calfw-week-end-date   end-date)))))))

(defun calfw--view-model-make-common-data-for-days (model begin-date end-date)
  "Return a MODEL object for linear views of days between BEGIN-DATE and END-DATE."
  (calfw--model-create-updated-view-data
   model
   (calfw--view-model-make-common-data
    model begin-date end-date
    `((headers . ,(calfw--view-model-make-day-names-for-days begin-date end-date)) ; a list of the index of day-of-week
      (days . ,(calfw--view-model-make-days ; a list of days, which corresponds to the index of `headers'
                begin-date end-date))))))



;;; view-month

(defun calfw--view-month-model (model)
  "Create a logical view model of monthly calendar from MODEL."
  (let* ((init-date (calfw--k 'init-date model))
         (year (calendar-extract-year init-date))
         (month (calendar-extract-month init-date))
         (begin-date (calfw-date month 1 year))
         (end-date (calfw-date month (calendar-last-day-of-month month year) year)))
    ;; model
    (append
     (calfw--view-model-make-common-data-for-weeks model begin-date end-date)
     `((month . ,month) (year . ,year)))))

(defun calfw--round-cell-width (width)
  "Adjust WIDTH to be a multiple of `calfw-fchar-horizontal-line' width."
  (cond
   ((eql (char-width calfw-fchar-horizontal-line) 1) width)
   (t (- width (% width (char-width calfw-fchar-horizontal-line))))))

(defun calfw--view-month-calc-param (dest total-weeks)
  "Calculate cell size from DEST and TOTAL-WEEKS and return an alist of parameters."
  (let*
      ((win-width (calfw-dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 7, footer 1, margin 2 => 15
       (win-height (max 15 (- (calfw-dest-height dest) 15)))
       (junctions-width (* (char-width calfw-fchar-junction) 8)) ; weekdays+1
       (cell-width  (calfw--round-cell-width
                     (max 5 (/ (- win-width junctions-width) 7)))) ; weekdays
       (cell-height (max 2 (/ win-height total-weeks))) ; max weeks = 6
       (total-width (+ (* cell-width calfw-week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,calfw-week-days))))

(defun calfw--view-month (component)
  "Render monthly calendar view.

Render the monthly calendar view for COMPONENT."
  (let* ((dest (calfw-component-dest component))
         (model (calfw--view-month-model (calfw-component-model component)))
         (total-weeks (length (calfw--k 'weeks model)))
         (param (calfw--render-append-parts
                 (calfw--view-month-calc-param dest total-weeks)))
         (total-width (calfw--k 'total-width param))
         (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
         (hline (calfw--k 'hline param)) (cline (calfw--k 'cline param)))
    ;; update model
    (setf (calfw-component-model component) model)
    ;; header
    (insert
     (calfw--rt (calfw-render-title-month (calfw--k 'init-date model))
                'calfw-title-face)
     EOL (calfw--render-toolbar total-width 'month
                                'calfw-navi-previous-month-command
                                'calfw-navi-next-month-command)
     EOL hline)
    ;; day names
    (calfw--render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (let ((year (calfw--k 'year model))
          (month (calfw--k 'month model)))
      (calfw--render-calendar-cells-weeks
       model param
       (lambda (date week-day hday)
         (calfw--rt
          (format "%s" (calendar-extract-day date))
          (cond
           (hday 'calfw-sunday-face)
           ((not (calfw-month-year-contain-p month year date)) 'calfw-disable-face)
           (t (calfw--render-get-week-face week-day 'calfw-default-day-face)))))))
    ;; footer
    (insert (calfw--render-footer total-width (calfw--model-get-contents-sources model)))))



;;; view-week

(defun calfw--view-week-model (model)
  "Create a logical view model of weekly calendar from MODEL."
  (let* ((init-date (calfw--k 'init-date model))
         (begin-date (calfw-week-begin-date init-date))
         (end-date (calfw-week-end-date init-date)))
    (calfw--view-model-make-common-data-for-weeks model begin-date end-date)))

;; (calfw-view-week-model (calfw-model-abstract-new (calfw-date 1 1 2011) nil nil))

(defun calfw--view-week-calc-param (dest)
  "Calculate cell size from the reference size and return rendering parameters.

Return an alist of rendering parameters based on the size of DEST."
  (let*
      ((win-width (calfw-dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (calfw-dest-height dest) 10)))
       (junctions-width (* (char-width calfw-fchar-junction) 8))
       (cell-width  (calfw--round-cell-width
                     (max 5 (/ (- win-width junctions-width) 7))))
       (cell-height (max 2 win-height))
       (total-width (+ (* cell-width calfw-week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,calfw-week-days))))

(defun calfw--view-week (component)
  "Render weekly calendar view for COMPONENT.

Render the weekly calendar view based on the COMPONENT's model and
parameters.  The model contains the begin and end dates, and the
parameters specify the layout and formatting."
  (let* ((dest (calfw-component-dest component))
         (param (calfw--render-append-parts (calfw--view-week-calc-param dest)))
         (total-width (calfw--k 'total-width param))
         (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
         (hline (calfw--k 'hline param)) (cline (calfw--k 'cline param))
         (model (calfw--view-week-model (calfw-component-model component)))
         (begin-date (calfw--k 'begin-date model))
         (end-date (calfw--k 'end-date model)))
    ;; update model
    (setf (calfw-component-model component) model)
    ;; header
    (insert
     (calfw--rt
      (calfw-render-title-period begin-date end-date)
      'calfw-title-face)
     EOL (calfw--render-toolbar total-width 'week
                                'calfw-navi-previous-week-command
                                'calfw-navi-next-week-command)
     EOL hline)
    ;; day names
    (calfw--render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (calfw--render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (calfw--rt (format "%s" (calendar-extract-day date))
                  (if hday 'calfw-sunday-face
                    (calfw--render-get-week-face
                     week-day 'calfw-default-day-face)))))
    ;; footer
    (insert (calfw--render-footer total-width (calfw--model-get-contents-sources model)))))



;;; view-two-weeks

(defun calfw-view-two-weeks-model-adjust (model)
  "Adjust the begin date of the two-weeks MODEL."
  (let ((in-date (calfw--k 'init-date model)))
    (cond
     ((eq 'two-weeks (calfw--k 'type model))
      (let ((old-begin-date (calfw--k 'begin-date model))
            (old-end-date (calfw--k 'end-date model)))
        (cond
         ((calfw-date-between old-begin-date old-end-date in-date)
          in-date)
         ((calfw-date-between old-end-date (calfw-date-after old-end-date calfw-week-days) in-date)
          old-end-date)
         ((calfw-date-between (calfw-date-after old-begin-date (- calfw-week-days)) old-begin-date in-date)
          (calfw-date-after old-begin-date (- calfw-week-days)))
         (t in-date))))
     (t in-date))))

(defun calfw--view-two-weeks-model (model)
  "Create a logical view model of two-weeks calendar from MODEL."
  (let* ((init-date (calfw-view-two-weeks-model-adjust model))
         (begin-date (calfw-week-begin-date init-date))
         (end-date (calfw-date-after begin-date (1- (* 2 calfw-week-days)))))
    ;; model
    (append
     (calfw--view-model-make-common-data-for-weeks model begin-date end-date)
     `((type . two-weeks)))))

;; (calfw-view-two-weeks-model (calfw-model-abstract-new (calfw-date 1 1 2011) nil nil))

(defun calfw--view-two-weeks-calc-param (dest)
  "Calculate cell size from the reference size and return rendering parameters.

Calculate cell size from the reference size in DEST and return an
alist of rendering parameters."
  (let*
      ((win-width (calfw-dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 3, footer 1, margin 2 => 11
       (win-height (max 15 (- (calfw-dest-height dest) 11)))
       (junctions-width (* (char-width calfw-fchar-junction) 8))
       (cell-width  (calfw--round-cell-width
                     (max 5 (/ (- win-width junctions-width) 7))))
       (cell-height (max 2 (/ win-height 2)))
       (total-width (+ (* cell-width calfw-week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,calfw-week-days))))

(defun calfw--view-two-weeks (component)
  "Render two-weeks calendar view for COMPONENT.

Render two-weeks calendar view for COMPONENT.  The calendar is
rendered to the destination specified by the component.  The model
of the component is updated."
  (let* ((dest (calfw-component-dest component))
         (param (calfw--render-append-parts (calfw--view-two-weeks-calc-param dest)))
         (total-width (calfw--k 'total-width param))
         (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
         (hline (calfw--k 'hline param)) (cline (calfw--k 'cline param))
         (model (calfw--view-two-weeks-model (calfw-component-model component)))
         (begin-date (calfw--k 'begin-date model))
         (end-date (calfw--k 'end-date model)))
    ;; update model
    (setf (calfw-component-model component) model)
    ;; header
    (insert
     (calfw--rt
      (calfw-render-title-period begin-date end-date)
      'calfw-title-face)
     EOL (calfw--render-toolbar total-width 'two-weeks
                                'calfw-navi-previous-week-command
                                'calfw-navi-next-week-command)
     EOL hline)
    ;; day names
    (calfw--render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (calfw--render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (calfw--rt (format "%s" (calendar-extract-day date))
                  (if hday 'calfw-sunday-face
                    (calfw--render-get-week-face
                     week-day 'calfw-default-day-face)))))
    ;; footer
    (insert (calfw--render-footer total-width (calfw--model-get-contents-sources model)))))



;;; view-day

(defun calfw--view-day-calc-param (dest &optional num)
  "Calculate cell size from the reference size and return rendering parameters.

Calculate cell size from the reference size and return an alist of
rendering parameters for DEST.  NUM is the number of columns."
  (let*
      ((num (or num 1))
       (win-width (calfw-dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (calfw-dest-height dest) 10)))
       (junctions-width (* (char-width calfw-fchar-junction) (1+ num)))
       (cell-width  (calfw--round-cell-width
                     (max 3 (/ (- win-width junctions-width) num))))
       (cell-height win-height)
       (total-width (+ (* cell-width num) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,num))))

(defun calfw--view-day (component)
  "Render daily calendar view for COMPONENT."
  (let* ((dest (calfw-component-dest component))
         (param (calfw--render-append-parts (calfw--view-day-calc-param dest)))
         (total-width (calfw--k 'total-width param))
         (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
         (hline (calfw--k 'hline param)) (cline (calfw--k 'cline param))
         (current-date (calfw--k 'init-date (calfw-component-model component)))
         (model
          (calfw--view-model-make-common-data-for-days
           (calfw-component-model component) current-date current-date)))
    ;; update model
    (setf (calfw-component-model component) model)
    ;; header
    (insert
     (calfw--rt
      (calfw-render-title-day current-date)
      'calfw-title-face)
     EOL (calfw--render-toolbar total-width 'day
                                'calfw-navi-previous-day-command
                                'calfw-navi-next-day-command)
     EOL hline)
    ;; day names
    (calfw--render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (calfw--render-calendar-cells-days
     model param
     (lambda (date week-day hday)
       (calfw--rt (format "%s" (calendar-extract-day date))
                  (if hday 'calfw-sunday-face
                    (calfw--render-get-week-face
                     week-day 'calfw-default-day-face)))))
    ;; footer
    (insert (calfw--render-footer total-width (calfw--model-get-contents-sources model)))))

(defun calfw--render-calendar-cells-days (model param title-func &optional
                                                days content-fun do-weeks)
  "Insert calendar cells for the linear views using MODEL and PARAM.

Insert calendar cells for the linear views using MODEL, PARAM, and
TITLE-FUNC.  Optional DAYS, CONTENT-FUN, and DO-WEEKS are also used."
  (calfw--render-columns
   (cl-loop with cell-width      = (calfw--k 'cell-width param)
            with days            = (or days (calfw--k 'days model))
            with content-fun     = (or content-fun
                                       'calfw--render-event-days-overview-content)
            with holidays        = (calfw--k 'holidays model)
            with annotations     = (calfw--k 'annotations model)
            with headers         = (calfw--k 'headers  model)
            with raw-periods-all = (calfw--render-periods-stacks model)
            with sorter          = (calfw-model-get-sorter model)

            for date in days ; days columns loop
            for count from 0 below (length days)
            for hday         = (car (calfw--contents-get date holidays))
            for week-day     = (nth count headers)
            for ant          = (calfw--rt (calfw--contents-get date annotations)
                                          'calfw-annotation-face)
            for raw-periods  = (calfw--contents-get date raw-periods-all)
            for raw-contents = (calfw--render-sort-contents
                                (funcall content-fun
                                         (calfw-model-get-contents-by-date date model))
                                sorter)
            for prs-contents = (calfw--render-rows-prop
                                (append (if do-weeks
                                            (calfw--render-periods
                                             date week-day raw-periods cell-width)
                                          (calfw--render-periods-days
                                           date raw-periods cell-width))
                                        (mapcar 'calfw--render-default-content-face
                                                raw-contents)))
            for num-label = (if prs-contents
                                (format "(%s)"
                                        (+ (length raw-contents)
                                           (length raw-periods))) "")
            for tday = (concat
                        " " ; margin
                        (funcall title-func date week-day hday)
                        (if num-label (concat " " num-label))
                        (if hday (concat " " (calfw--rt (substring hday 0)
                                                        'calfw-holiday-face))))
            collect
            (cons date (cons (cons tday ant) prs-contents)))
   param))

(defun calfw--render-periods-days (date periods-stack cell-width)
  "Insert period texts.

When PERIODS-STACK is non-nil, insert period texts for DATE
according to CELL-WIDTH.
Return a list of strings representing the periods."
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (cl-loop for (_row (begin end content)) in stack
               for beginp = (equal date begin)
               for endp = (equal date end)
               for width = (- cell-width 2)
               for title = (calfw--render-truncate
                            (concat
                             (calfw-strtime begin) " - "
                             (calfw-strtime end) " : "
                             content) width t)
               collect
               (if content
                   (calfw--rt
                    (concat
                     (if beginp "(" " ")
                     (calfw--render-left width title ?-)
                     (if endp ")" " "))
                    (calfw--render-get-face-period content 'calfw-periods-face))
                 "")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;; Following functions assume that the current buffer is a calendar view.

(defun calfw--cursor-to-date (&optional pos)
  "Return the date at POS, or nil if none."
  (get-text-property (or pos (point)) 'cfw:date))

(defun calfw-cursor-to-nearest-date ()
  "Return the date at or nearest the cursor position in the calendar."
  (or (calfw--cursor-to-date)
      (let* ((r (lambda () (when (not (eolp)) (forward-char))))
             (l (lambda () (when (not (bolp)) (backward-char))))
             (u (lambda () (when (not (bobp)) (line-move 1))))
             (d (lambda () (when (not (eobp)) (line-move -1))))
             (dest (calfw-component-dest (calfw-cp-get-component)))
             get)
        (setq get (lambda (cmds)
                    (save-excursion
                      (if (null cmds) (calfw--cursor-to-date)
                        (ignore-errors
                          (funcall (car cmds)) (funcall get (cdr cmds)))))))
        (or (cl-loop for i in `((,d) (,r) (,u) (,l)
                                (,d ,r) (,d ,l) (,u ,r) (,u ,l)
                                (,d ,d) (,r ,r) (,u ,u) (,l ,l))
                     for date = (funcall get i)
                     if date return date)
            (cond
             ((> (/ (point-max) 2) (point))
              (calfw--find-first-date dest))
             (t (calfw--find-last-date dest)))))))

(defun calfw--find-first-date (dest)
  "Return the first date in the current buffer using DEST."
  (let ((pos (next-single-property-change
              (calfw-dest-point-min dest) 'cfw:date)))
    (and pos (calfw--cursor-to-date pos))))

(defun calfw--find-last-date (dest)
  "Return the last date in the current buffer using DEST."
  (let ((pos (previous-single-property-change
              (calfw-dest-point-max dest) 'cfw:date)))
    (and pos (calfw--cursor-to-date (1- pos)))))

(defun calfw--find-by-date (dest date)
  "Return a point where the text property `cfw:date' equals DATE in DEST.

If DATE is not found in DEST, return nil."
  (cl-loop with pos = (calfw-dest-point-min dest)
           with end = (calfw-dest-point-max dest)
           for next = (next-single-property-change pos 'cfw:date nil end)
           for text-date = (and next (calfw--cursor-to-date next))
           while (and next (< next end)) do
           (if (and text-date (equal date text-date))
               (cl-return next))
           (setq pos next)))

(defun calfw--find-all-by-date (dest date func)
  "Call FUNC with begin and end positions of text with ‘cfw:date' equal to DATE.

Call FUNC in each region of DEST where the text-property
‘cfw:date' is equal to DATE.  FUNC receives two arguments,
begin position and end position."
  (cl-loop with pos = (calfw-dest-point-min dest)
           with end = (calfw-dest-point-max dest)
           for next = (next-single-property-change pos 'cfw:date nil end)
           for text-date = (and next (calfw--cursor-to-date next))
           while (and next (< next end)) do
           (if (and text-date (equal date text-date))
               (let ((cend (next-single-property-change
                            next 'cfw:date nil end)))
                 (funcall func next cend)))
           (setq pos next)))

(defun calfw--find-item (dest date row-count)
  "Find the schedule item in DEST which have properties DATE and ROW-COUNT.

The parameters are compared to text properties `cfw:date' and
`cfw:row-count'.  Returns the position of the item, or nil if no
item is found."
  (cl-loop with pos = (calfw-dest-point-min dest)
           with end = (calfw-dest-point-max dest)
           with last-found = nil
           for next = (next-single-property-change pos 'cfw:date nil end)
           for text-date = (and next (calfw--cursor-to-date next))
           for text-row-count = (and next (get-text-property next 'cfw:row-count))
           while (and next (< next end)) do
           (when (and text-date (equal date text-date)
                      (eql row-count text-row-count))
             ;; this is needed item
             (cl-return next))
           (when (and text-date (equal date text-date)
                      text-row-count)
             ;; keep it to search bottom item
             (setq last-found next))
           (setq pos next)
           finally (if (and last-found (< row-count 0))
                       (cl-return last-found))))

(defun calfw-cp-goto-date (component date &optional force-move-cursor)
  "Go to DATE on COMPONENT.

If the current view doesn't contain DATE, update the view to
display DATE.  If FORCE-MOVE-CURSOR is non-nil, move the cursor."
  (let ((dest (calfw-component-dest component))
        (model (calfw-component-model component)))
    (unless (calfw-cp-displayed-date-p component date)
      (calfw--model-set-init-date date model)
      (calfw--cp-update component))
    (calfw--cp-move-cursor dest date force-move-cursor)))

(defun calfw-navi-goto-date (date)
  "Move the cursor to DATE.
If DATE is not included on the current calendar, this function changes the
calendar view."
  (let ((cp (calfw-cp-get-component)))
    (when cp
      (calfw-cp-goto-date cp date))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major Mode / Key bindings

(defvar calfw-calendar-mode-map
  (calfw--define-keymap
   '(
     ("<right>" . calfw-navi-next-day-command)
     ("f"       . calfw-navi-next-day-command)
     ("<left>"  . calfw-navi-previous-day-command)
     ("b"       . calfw-navi-previous-day-command)
     ("<down>"  . calfw-navi-next-week-command)
     ("n"       . calfw-navi-next-week-command)
     ("<up>"    . calfw-navi-previous-week-command)
     ("p"       . calfw-navi-previous-week-command)

     ;; Vi style
     ("l" . calfw-navi-next-day-command)
     ("h" . calfw-navi-previous-day-command)
     ("j" . calfw-navi-previous-week-command)
     ("k" . calfw-navi-next-week-command)
     ("^" . calfw-navi-goto-week-begin-command)
     ("$" . calfw-navi-goto-week-end-command)

     ("<"   . calfw-navi-previous-month-command)
     ;;("M-v" . calfw-navi-previous-month-command)
     (">"   . calfw-navi-next-month-command)
     ;;("C-v" . calfw-navi-next-month-command)
     ("<prior>" . calfw-navi-previous-month-command)
     ("<next>"  . calfw-navi-next-month-command)
     ("<home>"  . calfw-navi-goto-first-date-command)
     ("<end>"   . calfw-navi-goto-last-date-command)

     ("M-g" . calfw-navi-goto-date-command)
     ("t" . calfw-navi-goto-today-command)
     ("." . calfw-navi-goto-today-command)

     ("TAB"       . calfw-navi-next-item-command)
     ("C-i"       . calfw-navi-next-item-command)
     ("<backtab>"   . calfw-navi-prev-item-command)
     ("S-TAB"       . calfw-navi-prev-item-command)

     ("g"   . calfw-refresh-calendar-buffer)
     ("SPC" . calfw-show-details-command)

     ("D" . calfw-change-view-day)
     ("W" . calfw-change-view-week)
     ("T" . calfw-change-view-two-weeks)
     ("M" . calfw-change-view-month)

     ([mouse-1] . calfw-navi-on-click)

     ("q" . bury-buffer)

     ("0" . digit-argument)
     ("1" . digit-argument)
     ("2" . digit-argument)
     ("3" . digit-argument)
     ("4" . digit-argument)
     ("5" . digit-argument)
     ("6" . digit-argument)
     ("7" . digit-argument)
     ("8" . digit-argument)
     ("9" . digit-argument)))
  "Default key map of calendar views.")

(defun calfw-calendar-mode-map (&optional custom-map)
  "Return a keymap object for the calendar buffer.

If CUSTOM-MAP is provided, set its parent to
`calfw-calendar-mode-map' and return CUSTOM-MAP.  Otherwise,
return `calfw-calendar-mode-map'."
  (cond
   (custom-map
    (set-keymap-parent custom-map calfw-calendar-mode-map)
    custom-map)
   (t calfw-calendar-mode-map)))

(defvar calfw-calendar-mode-hook nil
  "This hook is called at end of setting up major mode `calfw-calendar-mode'.")

(defun calfw-calendar-mode (&optional custom-map)
  "Set up `calfw-calendar-mode' as the major mode, using CUSTOM-MAP."

  ;; \\{calfw-calendar-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map (calfw-calendar-mode-map custom-map))
  (setq major-mode 'calfw-calendar-mode
        mode-name "Calendar Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'calfw-calendar-mode-hook))

;;; Actions

(defun calfw-change-view-month ()
  "Change the view of the current component to \\='month."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'month)))

(defun calfw-change-view-week ()
  "Change current component’s view to \\='week.
Changes the view of the current component, as returned by
`calfw-cp-get-component', to ‘week."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'week)))

(defun calfw-change-view-two-weeks ()
  "Change the view of the calendar component to \\='two-weeks."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'two-weeks)))

(defun calfw-change-view-day ()
  "Change the view of the current component to \\='day.
Changes the view of the current component, obtained via
`calfw-cp-get-component', to `day' by calling
`calfw-cp-set-view'."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'day)))

(defun calfw-navi-next-item-command ()
  "Move the cursor to the next item."
  (interactive)
  (let ((cp (calfw-cp-get-component))
        (date (calfw--cursor-to-date))
        (rcount (or (get-text-property (point) 'cfw:row-count) -1)))
    (when (and cp date)
      (let ((next (calfw--find-item (calfw-component-dest cp) date (1+ rcount))))
        (if next (goto-char next)
          (calfw-navi-goto-date date))))))

(defun calfw-navi-prev-item-command ()
  "Move the cursor to the previous item."
  (interactive)
  (let ((cp (calfw-cp-get-component))
        (date (calfw--cursor-to-date))
        (rcount (or (get-text-property (point) 'cfw:row-count) -1)))
    (when (and cp date)
      (let ((next (calfw--find-item (calfw-component-dest cp) date (1- rcount))))
        (if next (goto-char next)
          (calfw-navi-goto-date date))))))

(defun calfw-navi-on-click ()
  "Click on the date at point in the calendar."
  (interactive)
  (let ((cp (calfw-cp-get-component))
        (date (calfw--cursor-to-date)))
    (when (and cp date)
      (calfw-cp-goto-date cp date)
      (calfw--cp-fire-click-hooks cp))))

(defun calfw-refresh-calendar-buffer (no-resize)
  "Clear the calendar and render again.
With prefix arg NO-RESIZE, don't fit calendar to window size."
  (interactive "P")
  (let ((cp (calfw-cp-get-component)))
    (when cp
      (unless no-resize
        (calfw-cp-resize cp (window-width) (window-height)))
      (cl-loop for s in (calfw-cp-get-contents-sources cp t)
               for f = (calfw-source-update s)
               if f do (funcall f))
      (cl-loop for s in (calfw-cp-get-annotation-sources cp)
               for f = (calfw-source-update s)
               if f do (funcall f))
      (calfw--cp-update cp))))

(defun calfw-navi-goto-week-begin-command ()
  "Move the cursor to the first day of the current week."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-navi-goto-date
     (calfw-week-begin-date
      (calfw-cursor-to-nearest-date)))))

(defun calfw-navi-goto-week-end-command ()
  "Move the cursor to the last day of the current week."
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-navi-goto-date
     (calfw-week-end-date
      (calfw-cursor-to-nearest-date)))))

(defun calfw-navi-goto-date-command ()
  "Move the cursor to the specified date."
  (interactive)
  (calfw-navi-goto-date (call-interactively calfw-read-date-command)))

(defun calfw-navi-goto-today-command ()
  "Move the cursor to today."
  (interactive)
  (calfw-navi-goto-date (calfw-emacs-to-calendar (current-time))))

(defun calfw-navi-next-day-command (&optional num)
  "Move the cursor forward NUM days.  If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive "p")
  (when (calfw-cp-get-component)
    (unless num (setq num 1))
    (let* ((cursor-date (calfw-cursor-to-nearest-date))
           (new-cursor-date (calfw-date-after cursor-date num)))
      (calfw-navi-goto-date new-cursor-date))))

(defun calfw-navi-previous-day-command (&optional num)
  "Move the cursor back NUM days.  If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive "p")
  (calfw-navi-next-day-command (- (or num 1))))

(defun calfw-navi-goto-first-date-command ()
  "Move the cursor to the first day on the current calendar view."
  (interactive)
  (calfw-navi-goto-date
   (calfw--find-first-date
    (calfw-component-dest (calfw-cp-get-component)))))

(defun calfw-navi-goto-last-date-command ()
  "Move the cursor to the last day on the current calendar view."
  (interactive)
  (calfw-navi-goto-date
   (calfw--find-last-date
    (calfw-component-dest (calfw-cp-get-component)))))

(defun calfw-navi-next-week-command (&optional num)
  "Move the cursor forward NUM weeks.  If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive "p")
  (calfw-navi-next-day-command (* calfw-week-days (or num 1))))

(defun calfw-navi-previous-week-command (&optional num)
  "Move the cursor back NUM weeks.  If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive "p")
  (calfw-navi-next-day-command (* (- calfw-week-days) (or num 1))))

(defun calfw-navi-next-month-command (&optional num)
  "Move the cursor forward NUM months.  If NUM is nil, 1 is used.
Movement is backward if NUM is negative."
  (interactive "p")
  (when (calfw-cp-get-component)
    (unless num (setq num 1))
    (let* ((cursor-date (calfw-cursor-to-nearest-date))
           (month (calendar-extract-month cursor-date))
           (day   (calendar-extract-day   cursor-date))
           (year  (calendar-extract-year  cursor-date))
           (last (progn
                   (calendar-increment-month month year num)
                   (calendar-last-day-of-month month year)))
           (day (min last day))
           (new-cursor-date (calfw-date month day year)))
      (calfw-navi-goto-date new-cursor-date))))

(defun calfw-navi-previous-month-command (&optional num)
  "Move the cursor back NUM months.  If NUM is nil, 1 is used.
Movement is forward if NUM is negative."
  (interactive "p")
  (calfw-navi-next-month-command (- (or num 1))))

;;; Detail popup

(defun calfw-show-details-command ()
  "Show details on the nearest date."
  (interactive)
  (let* ((cursor-date (calfw-cursor-to-nearest-date))
         (cp  (calfw-cp-get-component))
         (model (and cp (calfw-component-model cp))))
    (when model
      (calfw-details-popup
       (calfw-details-layout cursor-date model)))))

(defvar calfw-details-buffer-name "*calfw-details*"
  "Name of details buffer.")
(defvar calfw-details-window-size 20
  "Default detail buffer window size.")

(defvar calfw-before-win-num)
(defvar calfw-main-buf)

(defun calfw-details-popup (text)
  "Popup the buffer to show details.
TEXT is a content to show."
  (let ((buf (get-buffer calfw-details-buffer-name))
        (before-win-num (length (window-list)))
        (main-buf (current-buffer)))
    (unless (and buf (eq (buffer-local-value 'major-mode buf)
                         'calfw-details-mode))
      (setq buf (get-buffer-create calfw-details-buffer-name))
      (with-current-buffer buf
        (calfw-details-mode)
        (set (make-local-variable 'calfw-before-win-num) before-win-num)))
    (with-current-buffer buf
      (let (buffer-read-only)
        (set (make-local-variable 'calfw-main-buf) main-buf)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun calfw-details-layout (date model)
  "Layout details and return the text.
DATE is a date to show.  MODEL is model object."
  (let* ((EOL "\n")
         (HLINE (calfw--rt (concat (make-string (window-width) ?-) EOL) 'calfw-grid-face))
         (holiday (calfw-model-get-holiday-by-date date model))
         (annotation (calfw-model-get-annotation-by-date date model))
         (periods (calfw-model-get-periods-by-date date model))
         (contents (calfw--render-sort-contents
                    (calfw--render-event-details-content
                     (calfw-model-get-contents-by-date date model))
                    (calfw-model-get-sorter model)))
         (row-count -1))
    (concat
     (calfw--rt (concat "Schedule on " (calfw-strtime date) " (") 'calfw-header-face)
     (calfw--rt (calendar-day-name date)
                (calfw--render-get-week-face (calendar-day-of-week date) 'calfw-header-face))
     (calfw--rt (concat ")" EOL) 'calfw-header-face)
     (when (or holiday annotation)
       (concat
        (and holiday (calfw--rt holiday 'calfw-holiday-face))
        (and holiday annotation " / ")
        (and annotation (calfw--rt annotation 'calfw-annotation-face))
        EOL))
     HLINE
     (cl-loop for (begin end summary) in periods
              for prefix = (propertize
                            (concat (calfw-strtime begin) " - " (calfw-strtime end) " : ")
                            'face (calfw--render-get-face-period summary 'calfw-periods-face)
                            'font-lock-face (calfw--render-get-face-period summary 'calfw-periods-face)
                            'cfw:row-count (cl-incf row-count))
              concat
              (concat prefix " " summary EOL))

     (cl-loop for i in contents
              for f = (calfw--render-get-face-content i 'calfw-default-content-face)
              concat
              (concat "- " (propertize
                            i 'face f 'font-lock-face f
                            'cfw:row-count (cl-incf row-count))
                      EOL)))))

(defvar calfw-details-mode-map
  (calfw--define-keymap
   '(("q"       . calfw-details-kill-buffer-command)
     ("SPC"     . calfw-details-kill-buffer-command)
     ("n"       . calfw-details-navi-next-command)
     ("f"       . calfw-details-navi-next-command)
     ("<right>" . calfw-details-navi-next-command)
     ("p"       . calfw-details-navi-prev-command)
     ("b"       . calfw-details-navi-prev-command)
     ("<left>"  . calfw-details-navi-prev-command)
     ("TAB"     . calfw-details-navi-next-item-command)
     ("C-i"     . calfw-details-navi-next-item-command)
     ("<backtab>" . calfw-details-navi-prev-item-command)
     ("S-TAB"     . calfw-details-navi-prev-item-command)))
  "Default key map for the details buffer.")

(defvar calfw-details-mode-hook nil)

(defun calfw-details-mode ()
  "Set up major mode `calfw-details-mode'.

\\{calfw-details-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map calfw-details-mode-map)
  (setq major-mode 'calfw-details-mode
        mode-name "Calendar Details Mode")
  (setq buffer-undo-list t
        buffer-read-only t)
  (run-hooks 'calfw-details-mode-hook))

(defun calfw-details-kill-buffer-command ()
  "Kill buffer and delete window."
  (interactive)
  (let ((win-num (length (window-list)))
        (next-win (get-buffer-window calfw-main-buf)))
    (when (and (not (one-window-p))
               (> win-num calfw-before-win-num))
      (delete-window))
    (kill-buffer calfw-details-buffer-name)
    (when next-win (select-window next-win))))

(defun calfw-details-navi-next-command (&optional num)
  "Go to the next day in the calendar and show its details.

Go to the next day in the calendar buffer, according to NUM,
and show its details in the details buffer."
  (interactive "p")
  (when calfw-main-buf
    (with-current-buffer calfw-main-buf
      (calfw-navi-next-day-command num)
      (calfw-show-details-command))))

(defun calfw-details-navi-prev-command (&optional num)
  "Go to the previous day in the calendar and show its details.

Goes back NUM days if NUM is provided."
  (interactive "p")
  (when calfw-main-buf
    (with-current-buffer calfw-main-buf
      (calfw-navi-previous-day-command num)
      (calfw-show-details-command))))

(defun calfw-details-navi-next-item-command ()
  "Go to the next item in the calfw details view."
  (interactive)
  (let* ((rcount (or (get-text-property (point) 'cfw:row-count) -1))
         (next-pos (calfw--details-find-item (1+ rcount))))
    (goto-char (or next-pos (point-min)))))

(defun calfw-details-navi-prev-item-command ()
  "Go to the previous item in the calfw details buffer."
  (interactive)
  (let* ((rcount (or (get-text-property (point) 'cfw:row-count) -1))
         (next-pos (calfw--details-find-item (1- rcount))))
    (goto-char (or next-pos (point-min)))))

(defun calfw--details-find-item (row-count)
  "Find the schedule item which has a specific ROW-COUNT.

ROW-COUNT is compared against the `cfw:row-count' property.  Returns
the position of the item, or nil if no item is found."
  (cl-loop with pos = (point-min)
           for next-pos = (next-single-property-change pos 'cfw:row-count)
           for text-row-count = (and next-pos (get-text-property next-pos
                                                                 'cfw:row-count))
           while next-pos do
           (when (eql row-count text-row-count)
             (cl-return next-pos))
           (setq pos next-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level API

;; buffer

(cl-defun calfw-open-calendar-buffer
    (&key date buffer custom-map contents-sources annotation-sources view sorter)
  "Open a calendar buffer.

DATE is the initial focus date, BUFFER is the buffer to use,
CUSTOM-MAP is the keymap, CONTENTS-SOURCES are the sources for
contents, ANNOTATION-SOURCES are the sources for annotations, VIEW
is the view to use, and SORTER is the sorter to use."
  (interactive)
  (let (cp)
    (save-excursion
      (setq cp (calfw-create-calendar-component-buffer
		:date date :buffer buffer :custom-map custom-map
		:contents-sources contents-sources
		:annotation-sources annotation-sources :view view :sorter sorter)))
    (switch-to-buffer (calfw-cp-get-buffer cp))))

(cl-defun calfw-create-calendar-component-buffer
    (&key date buffer custom-map contents-sources annotation-sources view sorter)
  "Return a calendar buffer with some customize parameters.

This function binds the component object at the
buffer local variable `calfw-component'.

The size of calendar is calculated from the window that shows
BUFFER or the selected window.  DATE is initial focus date.  If it
is nil, today is selected initially.  BUFFER is the buffer to be
rendered.  If BUFFER is nil, this function creates a new buffer
named `calfw-calendar-buffer-name'.  CUSTOM-MAP is the additional
keymap that is added to default keymap `calfw-calendar-mode-map'."
  (let* ((dest  (calfw-dest-init-buffer buffer nil nil custom-map))
         (model (calfw-model-abstract-new date contents-sources annotation-sources sorter))
         (cp (calfw--cp-new dest model view date)))
    (with-current-buffer (calfw-dest-buffer dest)
      (set (make-local-variable 'calfw-component) cp))
    cp))

;; region

(cl-defun calfw-create-calendar-component-region
    (&key date width height keymap contents-sources annotation-sources view sorter)
  "Display the calendar view at DATE.

This function also inserts markers of the rendering destination at
current point and returns a component object and stores it at the
text property `cfw:component'.

DATE is initial focus date.  If it is nil, today is selected
initially.  WIDTH and HEIGHT are reference size of the calendar
view as specified by VIEW.  If those are nil, the size is
calculated from the selected window.  CONTENTS-SOURCES,
ANNOTATION-SOURCES, and SORTER are used to construct the calendar
using `calfw-model-abstract-new' KEYMAP is the keymap that is put
to the text property `keymap'.  If KEYMAP is nil,
`calfw-calendar-mode-map' is used."
  (let (mark-begin mark-end)
    (setq mark-begin (point-marker))
    (insert " ")
    (setq mark-end (point-marker))
    (save-excursion
      (let* ((dest (calfw-dest-init-region (current-buffer) mark-begin mark-end width height))
             (model (calfw-model-abstract-new date contents-sources annotation-sources sorter))
             (cp (calfw--cp-new dest model view date))
             (after-update-func
              (let ((keymap keymap) (cp cp))
                (lambda ()
                  (calfw-dest-with-region (calfw-component-dest cp)
                                          (let (buffer-read-only)
                                            (put-text-property (point-min) (1- (point-max))
                                                               'cfw:component cp)
                                            (calfw--fill-keymap-property
                                             (point-min) (1- (point-max))
                                             (or keymap calfw-calendar-mode-map))))))))
        (setf (calfw-dest-after-update-func dest) after-update-func)
        (funcall after-update-func)
        cp))))

(defun calfw--fill-keymap-property (begin end keymap)
  "Set the KEYMAP text property to the region between BEGIN and END.

If the text already has some keymap property, the text is skipped."
  (save-excursion
    (goto-char begin)
    (cl-loop with pos = begin with nxt = nil
             until (or (null pos) (<= end pos))
             when (get-text-property pos 'keymap) do
             (setq pos (next-single-property-change pos 'keymap))
             else do
             (setq nxt (next-single-property-change pos 'keymap))
             (when (null nxt) (setq nxt end))
             (put-text-property pos (min nxt end) 'keymap keymap))))

;; inline

(cl-defun calfw-get-calendar-text
    (width height &key date _keymap contents-sources annotation-sources view sorter)
  "Return a text that draws the calendar view.

WIDTH and HEIGHT are reference size of the calendar view.
CONTENTS-SOURCES, ANNOTATION-SOURCES, VIEW, and SORTER are used
to construct the calendar using `calfw-model-abstract-new'.  DATE
is the initial focus date, or today if nil."
  (let* ((dest (calfw-dest-init-inline width height))
         (model (calfw-model-abstract-new date contents-sources annotation-sources sorter))
         (cp (calfw--cp-new dest model view date))
         text)
    (setq text
          (with-current-buffer (calfw-cp-get-buffer cp)
            (buffer-substring (point-min) (point-max))))
    (kill-buffer (calfw-cp-get-buffer cp))
    text))



;;; debug

(defun calfw-open-debug-calendar ()
  "Create a calendar buffer with some sample data for debugging purposes."
  (let* ((source1
          (make-calfw-source
           :name "test1"
           :color "Lightpink3"
           :period-bgcolor "Lightpink1"
           :period-fgcolor "White"
           :opt-face '(:weight bold)
           :opt-period-face '(:slant italic)
           :data
           (lambda (_b _e)
             '(((1  1 2011) "A happy new year!")
               ((1 10 2011) "TEST2" "TEST3")
               (periods
                ((1 8 2011) (1 9 2011) "Range1")
                ((1 11 2011) (1 12 2011) "[Sample]Range2 1/8-1/9")
                ((1 12 2011) (1 14 2011) "long long title3"))))
           :update
           (lambda () (message "SOURCE: test1 update!"))))
         (source2
          (make-calfw-source
           :name "test2"
           :data
           (lambda (_b _e)
             '(((1  2 2011) "The quick brown fox jumped over the lazy dog. The internationalization and Localization are long words.")
               ((1 10 2011) "PTEST2 title subject" "PTEST3 multi-line sample")
               (periods
                ((1 14 2011) (1 15 2011) "Stack")
                ((1 29 2011) (1 31 2011) "PERIOD W"))))))
         (asource1
          (make-calfw-source
           :name "Moon"
           :data
           (lambda (_b _e)
             '(((1  4 2011) . "New Moon")
               ((1 12 2011) . "Young Moon")
               ((1 20 2011) . "Full Moon")
               ((1 26 2011) . "Waning Moon")))))
         (asource2
          (make-calfw-source
           :name "Moon"
           :data
           (lambda (_b _e)
             '(((1  5 2011) . "AN1")
               ((1 13 2011) . "AN2")
               ((1 20 2011) . "AN3")
               ((1 28 2011) . "AN4")))))
         (event-source
          (make-calfw-source
           :name "Events"
           :color "DarkOrange"
           :data
           (lambda (_b _e)
             `(,(make-calfw-event :title       "Shopping"
                                  :start-date  '(1 17 2011))
               ,(make-calfw-event :title       "Other Thing"
                                  :start-date  '(1 17 2011))
               ,(make-calfw-event :title       "Spring cleaning"
                                  :start-date  '(1 15 2011)
                                  :location    "Home"
                                  :description "Oh what a joy!!")
               ,(make-calfw-event :title       "Meeting"
                                  :start-date  '(1 16 2011)
                                  :start-time  '(15 00)
                                  :location    "Office"
                                  :description "Important talk")
               ,(make-calfw-event :title       "Lunch"
                                  :start-date  '(1 15 2011)
                                  :start-time  '(13 15)
                                  :end-time    '(14 30)
                                  :location    "Fancy place"
                                  :description "Omnomnom")
               ,(make-calfw-event :title       "Long one"
                                  :start-date  '(1 17 2011)
                                  :description "This is a multiline description.

Some text here.

But also some here.

And here.")
               (periods
                ,(make-calfw-event :title      "Vacation bla bli blubb very long"
                                   :start-date '(1 13 2011)
                                   :end-date   '(1 20 2011)
                                   :location    "Beach"
                                   :description "Enjoy the sun!"))))))
         (cp (calfw-create-calendar-component-buffer
              :date (calfw-date 1 10 2011)
              :view 'two-weeks
              :contents-sources (list source1 source2 event-source)
              :annotation-sources (list asource1 asource2))))
    (calfw-cp-add-update-hook cp (lambda () (message "CALFW- UPDATE HOOK")))
    (calfw-cp-add-click-hook cp (lambda () (message "CALFW- CLICK HOOK %S" (calfw-cursor-to-nearest-date))))
    (switch-to-buffer (calfw-cp-get-buffer cp))))

(provide 'calfw)
;;; calfw.el ends here

;; (progn (eval-buffer) (calfw-open-debug-calendar))
;; (progn (eval-buffer) (calfw-open-calendar-buffer))
