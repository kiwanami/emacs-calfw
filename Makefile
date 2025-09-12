# Taken from radian
SHELL := bash

ELFILES := $(wildcard *.el)
ELCS    := $(ELFILES:.el=.elc)
VERSION ?=
CMD ?=

PACKAGE_LINT=@(emacs --batch --eval '(princ (file-name-directory (locate-library "package-lint")))')
PACKAGE_LINT="$(HOME)/.config/emacs/.elocal/straight/repos/package-lint/"

.PHONY: all
all: $(ELCS)

%.elc: %.el
	@echo ">>> Processing $< ..."
	# checkdoc
	-@emacs --batch -L . \
	  --eval "(require 'checkdoc)" \
	  --eval "(checkdoc-file \"$<\")"

	# Package lint
	-@emacs --batch -L "${PACKAGE_LINT}" \
		--eval "(with-temp-buffer (insert-file-contents \"$<\") \
(emacs-lisp-mode) (require 'package-lint) (package-lint-buffer))";

	# checkindent
	-@tmpdir="$$(mktemp -d)"; \
	emacs --batch \
	    --eval "(find-file \"$<\") (emacs-lisp-mode)" \
	    --eval "(let ((inhibit-message t)) (indent-region (point-min) (point-max)))" \
	    --eval "(write-file \"$$tmpdir/indented.el\")"; \
	(diff <(cat "$<" | nl -v1 -ba | sed 's/\t/: /') \
	      <(cat "$$tmpdir/indented.el" | nl -v1 -ba | sed 's/\t/: /')) \
	| grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true

	@# longlines
	@#-@awk '(length($$0) >= 80 && $$0 !~ /https?:\/\//) { printf "%s:%d: %s\n", FILENAME, NR, $$0 }' "$<" | (! grep .)

	# compile
	-@emacs --batch -L . -f batch-byte-compile $<

	# clean
	-@rm -f $@

.PHONY: clean
clean: ## Remove build artifacts
	@rm -f *.elc
