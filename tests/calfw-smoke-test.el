;;; calfw-smoke-test.el --- Smoke test: dump org sources to text and diff  -*- lexical-binding: t; -*-

;;; Commentary:

;; Writes two org fixtures from the strings below to a temporary directory,
;; opens a calfw calendar on each (once through the org-agenda-style source
;; and once through the file source), dumps each calendar to a text file
;; into the current directory, and checks that the multiday event shows up in both.
;;
;; Run with:
;;   emacs --batch -L . -L tests/ -l tests/calfw-smoke-test.el --eval "(calfw-smoke-test-run)"

;;; Code:

(require 'ert)
(require 'calfw)
(require 'calfw-org)

(defconst calfw-smoke-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory this test file lives in.")

(defconst calfw-smoke-test--org-content
  "* Event 1 this text make the title longer
<2026-11-03 Tue 13:00-14:00>
* Event 2
<2026-11-03 Tue 18:00-19:00>
* Point Event 1
<2026-11-03 Tue 22:00>
* Multiday Event
<2026-11-04 Wed>--<2026-11-06 Fri>
"
  "Contents of the org file used by the agenda source.")

(defun calfw-smoke-test--sources-to-file (sources output-file)
  "Open a calendar with SOURCES at a fixed size and write its text to OUTPUT-FILE."
  (let ((dims (cons 150 80)))
    (cl-letf (((symbol-function 'calfw-default-window-dims)
               (lambda (&optional _window) dims)))
      (calfw-open-calendar-buffer :date '(11 3 2026) :contents-sources sources)))
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-file output-file (insert text))
    (kill-buffer (current-buffer))))

(defun calfw-smoke-test--read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun calfw-smoke-test-run ()
  "Dump the create-source and file-source calendars and check the multiday event in both.
Signals an error, which makes `emacs --batch' exit non-zero, if a check fails."
  ;; Batch Emacs has no real default colors, so `calfw-composite-color'
  ;; would get "unspecified-bg"/"unspecified-fg" and fail.  Pin both so the
  ;; colors resolve.
  (set-face-background 'default "#000000")
  (set-face-foreground 'default "#ffffff")
  (let* ((create-source-org "tmp-create-source.org")
         (create-file-source-org "tmp-create-file-source-calendar.org")
         (create-source-out "create-source.cal")
         (create-file-source-out "create-file-source.cal"))
    (unwind-protect
        (progn
          (with-temp-file create-source-org (insert calfw-smoke-test--org-content))
          (with-temp-file create-file-source-org (insert calfw-smoke-test--org-content))
          (message "create-source-org=%s create-file-source-org=%s" create-source-org create-file-source-org)
          (calfw-smoke-test--sources-to-file
           (list (calfw-org-create-source (list create-source-org) "Org:my-calendar" "Seagreen4"))
           create-source-out)
          (calfw-smoke-test--sources-to-file
           (list (calfw-org-create-file-source "my-calendar" create-file-source-org "Seagreen4"))
           create-file-source-out)
          (message "%s" (with-temp-buffer
                          (call-process "diff" nil t nil "-u" create-source-out create-file-source-out)
                          (buffer-string)))))))

(ert-deftest calfw-smoke-test ()
  "Runs `calfw-smoke-test-run' so `make test' exercises the org source smoke test."
  (calfw-smoke-test-run))

(provide 'calfw-smoke-test)
;;; calfw-smoke-test.el ends here
