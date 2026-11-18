;;; utils.el --- Shared utils for calfw-org tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers used by the calfw-org-*-test.el files.  Not a test file itself.

;;; Code:

(require 'ert)
(require 'calfw-org)

(defmacro calfw-org-test--with-org (var content &rest body)
  "Bind VAR to a temporary .org file containing CONTENT, then run BODY.
The file is deleted, and any buffer visiting it is killed, even if BODY signals an error."
  (declare (indent 2))
  `(let ((,var (make-temp-file "calfw-org-test" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (when-let* ((buf (get-file-buffer ,var)))
         (kill-buffer buf))
       (delete-file ,var))))

(defun calfw-org-test--find (events title)
  "Return the first event in EVENTS whose title contains TITLE, or nil.
TITLE is matched literally, not as a regexp, against `calfw-event-title'."
  (cl-find-if (lambda (e)
                (string-match-p (regexp-quote title) (calfw-event-title e)))
              events))

(provide 'utils)
;;; utils.el ends here
