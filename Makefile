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

# Run make test to run all tests
# Run make test-<test-name> to run individual test
# Run make tests/<test-file>.el to run all tests in test file
TEST_FILES := $(wildcard tests/*-test.el)

# Names of the ERT tests defined in file $(1).
define get-test-names-from-test-file
$(shell grep -ho '^.ert-deftest [^ ]*' $(1) | cut -d' ' -f2)
endef

TEST_NAMES := $(foreach f,$(TEST_FILES),$(call get-test-names-from-test-file,$(f)))

# One `test-<testname>' target per ERT test, generated from the test files.
all-tests := $(addprefix test-,$(TEST_NAMES))

# One `test-<file>.el' target per test file (e.g. `test-calfw-org-test.el'),
# which runs every ERT test defined in that file.
all-test-files := $(TEST_FILES)

.PHONY: test $(all-tests) $(all-test-files)
test: $(all-tests) ## Run all ERT tests
$(all-tests): test-%:
	@emacs --batch -L . -L tests/ $(foreach f,$(TEST_FILES),-l $(f)) --eval "(ert-run-tests-batch-and-exit '$*)"
$(all-test-files): %:
	@emacs --batch -L . -L tests/ $(foreach f,$(TEST_FILES),-l $(f)) --eval "(ert-run-tests-batch-and-exit '(member $(call get-test-names-from-test-file,$*)))"
