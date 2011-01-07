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


;;; Code:

(require 'cl)
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

(defface cfw:face-regions
  '((((class color) (background light))
     :background "#668cd9" :foreground "White" :slant italic)
    (((class color) (background dark))
     :background "#d4e5ff" :foreground "RoyalBlue" :slant italic))
  "Face for region" :group 'calfw)

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

(defun cfw:mapsub (len lst)
  (when (vectorp lst)
    (setq lst (append lst nil))) ; vector to list
  (loop for i in lst collect
        (substring i 0 len)))

(defun cfw:k (key alist)
  (cdr (assq key alist)))

(defun cfw:rt (text face)
  (unless (stringp text) (setq text (format "%s" (or text ""))))
  (put-text-property 0 (length text) 'face face text)
  (put-text-property 0 (length text) 'font-lock-face face text)
  text)

(defun cfw:tp (text prop value)
  (if (< 0 (length text))
    (put-text-property 0 (length text) prop value text))
  text)

(defun cfw:define-keymap (keymap-list)
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
  (if (string-match "^[ \t\n\r]*\\(.*?\\)[ \t\n\r]*$" str)
      (match-string 1 str)
    str))



;;; Date Time Transformation

(defun cfw:date (month day year)
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
  "DATE1 と DATE2 の年月が同じであれば t。"
  (and 
   (= (calendar-extract-month date1)
      (calendar-extract-month date2))
   (= (calendar-extract-year date1)
      (calendar-extract-year date2))))

(defun cfw:date-less-equal-p (d1 d2)
  ""
  (let ((ed1 (cfw:calendar-to-emacs d1))
        (ed2 (cfw:calendar-to-emacs d2)))
    (or (equal ed1 ed2)
        (time-less-p ed1 ed2))))

(defun cfw:date-between (begin end date)
  "BEGIN と END の間（両端含む）にDATEがあればt。日付はカレンダー形式。"
  (and (cfw:date-less-equal-p begin date)
       (cfw:date-less-equal-p date end)))

(defun cfw:month-year-contain-p (month year date2)
  "MONTH / YEAR に DATE2 が含まれていれば t。"
  (and 
   (= month (calendar-extract-month date2))
   (= year (calendar-extract-year date2))))

(defun cfw:strtime-emacs (time)
  (format-time-string "%Y/%m/%d" time))

(defun cfw:strtime (date)
  "Format string form from calendar time format."
  (cfw:strtime-emacs (cfw:calendar-to-emacs date)))

(defun cfw:parsetime-emacs (str)
  (when (string-match "\\([0-9]+\\)\\/\\([0-9]+\\)\\/\\([0-9]+\\)" str)
     (apply 'encode-time 
            (let (ret)
              (dotimes (i 6)
                (push (string-to-number (or (match-string (+ i 1) str) "0")) ret))
              ret))))

(defun cfw:parsetime (str)
  (cfw:emacs-to-calendar (cfw:parsetime-emacs str)))

(defun cfw:enumerate-days (begin end)
  (when (> (calendar-absolute-from-gregorian begin)
           (calendar-absolute-from-gregorian end))
    (error "Invalid region period : %S - %S" begin end))
  (let ((d begin) ret (cont t))
    (while cont
      (push (copy-list d) ret)
      (setq cont (not (equal d end)))
      (setq d (calendar-gregorian-from-absolute
               (1+ (calendar-absolute-from-gregorian d)))))
    (nreverse ret)))

;;; Rendering destination

;; cfw:dest 描画先構造体
;; type 描画先の識別シンボル。buffer か region
;; buffer 描画先のバッファ
;; min-func 描画範囲の上限を返す関数
;; max-func 描画範囲の下限を返す関数
;; width カレンダーの描画サイズ。このサイズよりも小さくなる。
;; height カレンダーの描画サイズ。このサイズよりも小さくなる。
;; clear-func 描画範囲をクリアする関数。描画開始用フックとしても使える。
;; update-func 描画が終わったときに呼ばれる関数。nil可。

(defstruct cfw:dest type buffer min-func max-func width height clear-func update-func)

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

;; ○内容関数の仕様
;; 範囲の日付（両端含む）を受け取って、(日付 . 内容の文字列のリスト)のconsセルを返す
;; 順番は問わないが、日付のエントリーはユニークであること
;; 文字列はそのまま出力されるのでtextプロパティで修飾しておいて良い
;; 期間スケジュール
;; Input  : begin[DATE] end[DATE]
;; Output : '((DATE CONTENT1 CONTENT2 ...) (DATE CONTENT ... )
;;            (regions (DATE DATE CONTENT) (DATE DATE CONTENT) ... ))

(defvar cfw:contents-functions nil "期間からスケジュールのリストを返す関数のリスト")

;; ○アノテーション関数の仕様
;; 範囲の日付（両端含む）を受け取って、(日付 . 内容の文字列)のconsセルを返す
;; 複数指定された場合は適当に混ぜて表示される

(defvar cfw:annotations-functions nil "期間からアノテーションのリストを返す関数のリスト")

;; ○休日
;; calendar-holidays に日付を設定する。
;; `calendar-holidays'のdocstringを参照。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level API

;; buffer

(defun cfw:open-calendar-buffer (&optional date)
  "一番手っ取り早くカレンダーを表示するコマンド。
DATE省略時は今日の日付。"
  (interactive)
  (switch-to-buffer (cfw:get-calendar-buffer-custom date)))

(defun cfw:get-calendar-buffer-custom (&optional date buffer custom-map)
  "カレンダーのバッファを返す。
描画先オブジェクトはバッファローカル変数の`cfw:dest'に保存する。
サイズはBUFFERが表示されているウインドウサイズか、現在選択されているウインドウサイズ。
DATE省略時は今日の日付。
BUFFERはカレンダーを表示させたいバッファ。省略時は`cfw:calendar-buffer-name'を使う。
CUSTOM-MAPは標準の`cfw:calendar-mode-map'に追加したいキーマップ。"
  (let* ((dest (cfw:dest-init-buffer buffer nil nil custom-map))
         (buf (cfw:dest-buffer dest)))
    (cfw:calendar-update dest)
    (with-current-buffer buf
      (cfw:navi-goto-date (or date (calendar-current-date))))
    buf))

;; region

(defun cfw:insert-calendar-region (&optional date width height custom-map)
  "カレンダーのリージョンを入れて描画する。
描画先オブジェクトはバッファローカル変数の`cfw:dest'に保存する。（※このことから現状では1バッファにつき1つしかカレンダーを描画できない。）
サイズはBUFFERが表示されているウインドウサイズか、現在選択されているウインドウサイズ。
DATE省略時は今日の日付。
WIDTH, HEIGHTはカレンダーの参考サイズ。十分なサイズ（大体45x20程度）があればそれ以下のサイズ、十分なサイズでなければ最小限のサイズで描画する。
CUSTOM-MAPはカレンダーリージョン内のテキストに割り当てたいキーマップ（テキストプロパティの`keymap'に割り当てる）。脱出できるようなキーを入れていた方が良いかも。"
  (let (mark-begin mark-end dest)
    (setq mark-begin (point-marker))
    (insert "\n")
    (setq mark-end (point-marker))
    (save-excursion
      (setq dest (cfw:dest-init-region (current-buffer) mark-begin mark-end width height))
      (setf (cfw:dest-update-func dest) 
            (lexical-let ((custom-map custom-map) (dest dest))
              (lambda () 
                (cfw:dest-with-region dest
                  (let (buffer-read-only)
                    (put-text-property (point-min) (point-max)
                                       'keymap custom-map))))))
      (set (make-local-variable 'cfw:dest) dest)
      (cfw:calendar-update dest)
      (cfw:navi-goto-date (or date (calendar-current-date))))
    dest))

;; inline

(defun cfw:get-schedule-text (&optional date width height custom-map)
  "カレンダーが描画されたテキストを返す。カレンダーを単純に貼り付けたい場合向け。
描画先オブジェクトは使い捨てなので、自立して再描画できない。
DATE省略時は今日の日付。
WIDTH, HEIGHTはカレンダーの参考サイズ。十分なサイズ（大体45x20程度）があればそれ以下のサイズ、十分なサイズでなければ最小限のサイズで描画する。
CUSTOM-MAPはそのテキストに割り当てたいキーマップ（テキストプロパティの`keymap'に割り当てる）。"
  (let* ((dest (cfw:dest-init-inline width height))
         (buf (cfw:dest-buffer dest)) text)
    (cfw:calendar-update dest)
    (setq text
          (with-current-buffer buf
            (buffer-substring (point-min) (point-max))))
    (kill-buffer buf)
    (when custom-map
      (cfw:tp text 'keymap custom-map))
    text))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low level API

;; Buffer

(defconst cfw:calendar-buffer-name "*cfw-calendar*" "[internal]")

(defun cfw:dest-init-buffer (&optional buf width height custom-map)
  "カレンダーの描画先としてバッファ全体を使う。
カレンダー用のメジャーモードをセットし、キーバインドも設定する。
BUFはバッファ名。nilであれば `cfw:calendar-buffer-name' のバッファを生成。
サイズは指定されたバッファが表示されていればそのウインドウサイズを使用。
もしウインドウが見つからなければ、現在選択されているウインドウサイズを使用。
ここで作成した描画先構造体はバッファローカル変数 `cfw:dest' に格納される。
CUSTOM-MAPはこのバッファで使う追加のキーバインド。
キーバインドから呼ばれるアクションは、この変数があることを前提として動作する。"
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
  "カレンダーの描画先として指定されたバッファのマーク範囲の中を使う。
別のアプリの組み込みとしての使用を想定。mark-beginとmark-endの間には1文字以上（出来れば改行が望ましい）が必要。
メジャー（マイナー）モードやキーバインドは設定しない。
組み込むアプリ側でキーバインドを設定し、カーソール位置の属性やAPIを操作してカレンダーを利用する。
ここで作成した描画先構造体はアプリケーション側が管理する。アクションの関数はこの変数があることを前提として動作するため、letでダイナミック変数 `cfw:dest' に格納して呼ぶこと。
サイズは指定されたバッファが表示されていればそのウインドウサイズを使用。
もしウインドウが見つからなければ、現在選択されているウインドウサイズを使用。"
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
  "単純に描画したカレンダーのテキストを返す。"
  (lexical-let
      ((buffer (get-buffer-create cfw:dest-background-buffer))
       (window (selected-window))
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
    dest))


;;; Buffer and layout

(defvar cfw:calendar-update-after-hook nil "カレンダー描画後に呼ばれるフック")

(defun cfw:calendar-update (dest &optional month year)
  "バッファの内容を指定された年月で更新する"
  (let* ((today (calendar-current-date))
         (month (or month (calendar-extract-month today)))
         (year (or year (calendar-extract-year today)))
         (buf (cfw:dest-buffer dest)))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (cfw:dest-with-region dest 
          (cfw:navi-selection-clear)
          (cfw:dest-clear dest)
          (cfw:render-overlays-clear)
          (cfw:render-month 
           (cfw:model-month-create month year)
           (cfw:render-month-calc-param dest))
          (cfw:render-overlays-put)))
      (run-hooks 'cfw:calendar-update-after-hook)
      (cfw:dest-update dest))))



;;; Rendering

(defvar cfw:render-overlays nil "[internal]")
(make-variable-buffer-local 'cfw:render-overlays)

(defun cfw:render-overlays-clear ()
  (loop for i in cfw:render-overlays
        do (delete-overlay i))
  (setq cfw:render-overlays nil))

(defun cfw:render-overlays-put ()
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
  "中央寄せ"
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
  "左寄せで右パディング"
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string 
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat cnt (make-string margin padding))))

(defun cfw:render-right (width string &optional padding)
  "右寄せで左パディング"
  (let* ((padding (or padding ?\ ))
         (cnt (or (and string 
                       (cfw:render-truncate string width t))
                  ""))
         (len (string-width cnt))
         (margin (- width len)))
    (concat (make-string margin padding) cnt)))

(defun cfw:render-add-right (width left right &optional padding)
  "文字列leftの右側に空いた隙間に、右寄せで文字列rightを追加する"
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
  "画面サイズに合うようにサイズを計算する"
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
  "月のカレンダーの枠を描画する。描画方法だけに専念する。"
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
          with regions     = (cfw:render-layout-regions model)
          do
          (cfw:render-month-week
           (loop for day in week ; week columns loop 
                 for count from 0 below (length week)
                 for week-day = (nth count headers)
                 for date = (cfw:date month day year)
                 for hday = (car (cfw:contents-get date holidays))
                 for ant = (cfw:rt (cfw:contents-get date annotations) 'cfw:face-annotation)
                 for raw-regions = (cfw:contents-get date regions)
                 for raw-contents = (cfw:contents-get date contents)
                 for prs-contents = (append
                                     (cfw:render-regions date week-day raw-regions)
                                     (mapcar 'cfw:render-default-content-face raw-contents))
                 for num-label = (if prs-contents
                                     (format "(%s)" 
                                             (+ (length raw-contents)
                                                (length raw-regions))) "")
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
  "cfw:render-monthの内部関数。ローカル変数にreadonlyアクセス。
日ごとに縦に並んでいるリストを横につなげてレイアウトする。"
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
  (loop for i from 0 below (length str)
        with ret = (substring str 0)
        with face = (or default-face 'cfw:face-default-content)
        unless (get-text-property i 'face ret)
        do 
        (put-text-property i (1+ i) 'face face ret)
        (put-text-property i (1+ i) 'font-lock-face face ret)
        finally return ret))

(defun cfw:render-get-week-face (daynum &optional default-face)
  (cond
   ((= daynum cfw:week-saturday)
    'cfw:face-saturday)
   ((= daynum cfw:week-sunday)
    'cfw:face-sunday)
   (t default-face)))

(defun cfw:render-truncate (org limit-width &optional ellipsis)
  "limit-widthよりも長い内容はポップアップを追加する。"
  (if (< limit-width (string-width org))
      (let ((str (truncate-string-to-width 
                  (substring org 0) limit-width 0 nil ellipsis)))
        (cfw:tp str 'mouse-face 'highlight)
        (cfw:tp str 'help-echo org)
        str)
    org))

(defun cfw:render-regions (date week-day regions-stack)
  "cfw:render-monthの内部関数。ローカル変数にreadonlyアクセス。
regions-stackから描画用の内容に変換する。"
  (when regions-stack
    (let ((stack (sort regions-stack (lambda (a b) (< (car a) (car b))))))
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
                 'cfw:face-regions)
              "")))))

(defun cfw:render-layout-regions-get-min (regions-each-days begin end)
  "regions-each-daysの中から、beginとendの範囲で最小のrow番号を返す"
  (loop for row-num from 0 below 10 ; 期間が10個重なることはないと仮定
        unless
        (loop for d in (cfw:enumerate-days begin end)
              for regions-stack = (cfw:contents-get d regions-each-days)
              if (and regions-stack (assq row-num regions-stack))
              return t)
        return row-num))

(defun cfw:render-layout-regions-place (regions-each-days row region)
  "regions-each-daysにregionを割り当てる"
  (loop for d in (cfw:enumerate-days (car region) (cadr region))
        for regions-stack = (cfw:contents-get d regions-each-days)
        if regions-stack
        do (nconc regions-stack (list (list row region)))
        else
        do (push (cons d (list (list row region))) regions-each-days))
  regions-each-days)

(defun cfw:render-layout-regions (model)
  "regionsのデータから、日ごとの regions-stack -> ((row-num . region) ... ) を作る"
  (let* (regions-each-days)
    (loop for region in (cfw:k 'regions model)
          for (begin end content) = region
          for row = (cfw:render-layout-regions-get-min
                     regions-each-days begin end)
          do 
          (setq regions-each-days
                (cfw:render-layout-regions-place
                 regions-each-days row region)))
    regions-each-days))



;;; Models

(defun cfw:model-month-create (month year)
  "月のカレンダーの論理モデルを作成する。
表示内容や並び方についてはここで決定する。
内容だけに専念し、どのように描画されるかについては関知しない。"
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
                         unless (eq 'regions (car i)) 
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
    `((month . ,month)       ; 1から始まる月の数字
      (year . ,year)         ; 西暦
      (headers . ,day-names) ; 曜日のindex。描画側が何を使うかを決める。
      (holidays . ,holidays) ; (DATE 祝日名)のリスト
      (annotations . ,(cfw:annotations-merge begin-date end-date)) ; (DATE 内容)のリスト
      (contents . ,contents) ; (DATE 内容のリスト)のリスト
      (regions . ,(cfw:k 'regions contents-all)) ; (DATE DATE 内容)のリスト
      (weeks . ,(nreverse weeks)) ; 週ごとに日付の数字が並んでいる。headersの並びと対応。
      )))

(defun cfw:contents-get (date contents)
  "指定した日付の内容リストを取得する。"
  (cdr (cfw:contents-get-internal date contents)))

(defun cfw:contents-get-internal (date contents)
  "指定した日付の内容リストを取得する。
先頭がDATE、後続のリストが表示するべき内容。破壊的につなげる。"
  (cond
   ((or (null date) (null contents)) nil)
   (t (loop for i in contents
            if (equal date (car i))
            return i
            finally return nil))))

(defmacro cfw:contents-add (date content contents)
  "contentsに内容を追加する。マクロ注意。"
  (let (($prv (gensym)) ($lst (gensym))
        ($d (gensym)) ($c (gensym)))
    `(let* ((,$d ,date) (,$c ,content)
            (,$prv (cfw:contents-get-internal ,$d ,contents))
            (,$lst (if (listp ,$c) (copy-list ,$c) (list ,$c))))
       (if ,$prv (nconc ,$prv ,$lst)
         (push (cons ,$d ,$lst) ,contents)))))

(defun cfw:contents-merge (begin end)
  "指定した範囲の内容のリストを取ってくる。"
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
  "指定した範囲の内容のリストを取ってくる。"
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
          do (push (copy-list cnt) annotations)
          finally return annotations))))

(defun cfw:contents-debug-data ()
  (setq cfw:contents-functions
        (list
         (lambda (b e)
           '(((1  1 2011) "TEST1") 
             ((1 10 2011) "TEST2" "TEST3")
             (regions 
              ((1 8 2011) (1 9 2011) "REGION1")
              ((1 11 2011) (1 12 2011) "Region2")
              ((1 12 2011) (1 14 2011) "long long title3"))
             ))
         (lambda (b e) 
           '(((1  2 2011) "PTEST1") 
             ((1 10 2011) "PTEST2" "PTEST3")
             (regions 
              ((1 14 2011) (1 15 2011) "重ね合わせ")
              ((1 29 2011) (1 31 2011) "REGION W"))
             ))))
  (setq cfw:annotations-functions
        (list
         (lambda (b e)
           '(((1  4 2011) . "新月") 
             ((1 12 2011) . "上弦")
             ((1 20 2011) . "満月")
             ((1 26 2011) . "下弦")
             )))))



;;; Navigation

;; 以下の関数はカレントバッファにカレンダーが描画してあることが前提

(defun cfw:cursor-to-date (&optional pos)
  "カーソールのその場にある日付を取ってくる。見つからなかったらnil。"
  (get-text-property (or pos (point)) 'cfw:date))

(defun cfw:cursor-to-nearest-date ()
  "カーソールの近所のdateを取ってくる。近所に見つからなかったらバッファの先頭や後方から探す。
nilであることはあり得ない。nilの場合は探すバッファが間違ってる。"
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
  "バッファの一番先頭にある日付を取ってくる"
  (let ((pos (next-single-property-change (point-min) 'cfw:date)))
    (and pos (cfw:cursor-to-date pos))))

(defun cfw:find-last-date ()
  "バッファの一番後ろにある日付を取ってくる"
  (let ((pos (previous-single-property-change (point-max) 'cfw:date)))
    (and pos (cfw:cursor-to-date (1- pos)))))

(defun cfw:find-by-date (date)
  "DATEで指定された日付のpointを返す。バッファの先頭から探していくため、左上の場所。
見つからなかったらnilを返す。"
  (let ((pos (point-min)) begin ret)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq pos begin
            text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (setq ret begin
              pos (point-max))))
    ret))

(defun cfw:find-all-by-date (date func)
  "バッファの先頭からDATEで指定された日付のリージョンをすべてfuncに渡す。overlayを設置するなどに使う。FUNCは２つの引数（begin end）をとる関数。"
  (let ((pos (point-min)) begin text-date)
    (while (setq begin (next-single-property-change pos 'cfw:date))
      (setq text-date (cfw:cursor-to-date begin))
      (when (and text-date (equal date text-date))
        (let ((end (next-single-property-change 
                    begin 'cfw:date nil (point-max))))
          (funcall func begin end)))
      (setq pos begin))))

(defvar cfw:move-hook nil "選択状態にした後に呼ばれるフック。引数1つ（DATE）の関数。")
(make-variable-buffer-local 'cfw:navi-selection-overlays)

(defun cfw:navi-goto-date-internal (date)
  (goto-char (cfw:find-by-date date))
  (cfw:navi-selection-clear)
  (cfw:navi-selection-set date)
  (run-hook-with-args 'cfw:move-hook date))

(defvar cfw:navi-selection-overlays nil "[internal]")
(make-variable-buffer-local 'cfw:navi-selection-overlays)

(defun cfw:navi-selection-clear ()
  (loop for i in cfw:navi-selection-overlays
        do (delete-overlay i))
  (setq cfw:navi-selection-overlays nil))

(defun cfw:navi-selection-set (date)
  "DATEで指定した日付を選択状態にする。何度も呼んで複数つけることも出来る。
バッファ内にDATEが存在しない場合は何もしない。"
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
  "DATEの日付の日に移動して選択状態にする。"
  (unless (cfw:find-by-date date)
    (cfw:calendar-update cfw:dest
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

     ("g" . cfw:navi-goto-date-command)
     ("t" . cfw:navi-goto-today-command))))

(defun cfw:calendar-mode-map (&optional custom-map)
  (cond
   (custom-map
    (set-keymap-parent custom-map cfw:calendar-mode-map)
    custom-map)
   (t cfw:calendar-mode-map)))

(defvar cfw:calendar-mode-hook nil "バッファ初回設定時のメジャーモード設定後に呼ばれる。")

(defun cfw:calendar-mode (&optional custom-map)
  "メジャーモードを設定する"
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
  (interactive)
  (when cfw:dest
    (let ((date (or (cfw:cursor-to-nearest-date) 
                            (calendar-current-date))))
      (cfw:calendar-update cfw:dest)
      (cfw:navi-goto-date date))))

(defun cfw:navi-goto-week-begin-command ()
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (back-num (% (- (calendar-day-of-week cursor-date) 
                         calendar-week-start-day)
                      cfw:week-days)))
    (cfw:navi-previous-day-command back-num)))

(defun cfw:navi-goto-week-end-command ()
  (interactive)
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (forward-num (% (- cfw:week-saturday (calendar-day-of-week cursor-date)
                            calendar-week-start-day)
                         cfw:week-days)))
    (cfw:navi-next-day-command forward-num)))

(defun cfw:navi-goto-date-command (arg)
  "日付を手入力して移動する"
  (interactive "sInput Date (YYYY/MM/DD): ")
  (cfw:navi-goto-date (cfw:parsetime arg)))

(defun cfw:navi-goto-today-command ()
  (interactive)
  (cfw:navi-goto-date (cfw:emacs-to-calendar (current-time))))

(defun cfw:navi-next-day-command (&optional num)
  "カーソールのある日付の次の日付に移動する。
NUMは移動量。省略した場合は１。"
  (interactive)
  (unless num (setq num 1))
  (let* ((cursor-date (cfw:cursor-to-nearest-date))
         (new-cursor-date
          (calendar-gregorian-from-absolute
           (+ (calendar-absolute-from-gregorian cursor-date) num))))
    (cfw:navi-goto-date new-cursor-date)))

(defun cfw:navi-previous-day-command (&optional num)
  "カーソールのある日付の前の日付に移動する。
NUMは移動量。省略した場合は１。"
  (interactive)
  (cfw:navi-next-day-command (- (or num 1))))

(defun cfw:navi-goto-first-date-command ()
  "表示中のカレンダーの初日に移動する"
  (interactive)
  (cfw:navi-goto-date (cfw:find-first-date)))

(defun cfw:navi-goto-last-date-command ()
  "表示中のカレンダーの最終日に移動する"
  (interactive)
  (cfw:navi-goto-date (cfw:find-last-date)))

(defun cfw:navi-next-week-command (&optional num)
  "カーソールのある日付の次の週に移動する。
NUMは移動量。省略した場合は１。"
  (interactive)
  (cfw:navi-next-day-command (* cfw:week-days (or num 1))))

(defun cfw:navi-previous-week-command (&optional num)
  "カーソールのある日付の前の週に移動する。
NUMは移動量。省略した場合は１。"
  (interactive)
  (cfw:navi-next-day-command (* (- cfw:week-days) (or num 1))))

(defun cfw:navi-next-month-command (&optional num)
  "カーソールのある日付の次の月に移動する。
NUMは移動量。省略した場合は１。"
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
  "カーソールのある日付の前の週に移動する。
NUMは移動量。省略した場合は１。"
  (interactive)
  (cfw:navi-next-month-command (- (or num 1))))


(provide 'calfw)
;;; calfw.el ends here

;; (cfw:contents-debug-data)
;; (progn (eval-current-buffer) (cfw:open-calendar-buffer))