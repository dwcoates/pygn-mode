ifneq ($(CASK),)
 EMACS := $(CASK) emacs
else
 EMACS := emacs
 CASK := true
endif

EMACS_CLEAN := -Q
EMACS_BATCH := $(EMACS_CLEAN) --batch
WORK_DIR := $(shell pwd)
PACKAGE_NAME := $(shell basename $(WORK_DIR))
AUTOLOADS_FILE := $(PACKAGE_NAME)-autoloads.el

TEST_DIR := ert-tests
# TESTS can be overridden to specify a subset of tests
TESTS=

.PHONY : cask-install build autoloads test-autoloads test-tests test-prep test-batch test clean

cask-install :
	@$(CASK) install

build :
	$(EMACS) $(EMACS_BATCH) --eval             \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

autoloads :
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L . -l './$(AUTOLOADS_FILE)' || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-tests :
	@perl -ne 'if (m/^\s*\(\s*ert-deftest\s*(\S+)/) {die "$$1 test name duplicated in $$ARGV\n" if $$dupes{$$1}++}' '$(TEST_DIR)/'*-test.el

test-prep : cask-install build test-autoloads test-tests

test-batch :
	for test_lib in $(TEST_DIR)/*-test.el; do           \
	  $(EMACS) $(EMACS_BATCH) -L $(TEST_DIR) -L .       \
	  -l "$$test_lib" --eval                            \
	   "(progn                                          \
	     (fset 'ert--print-backtrace 'ignore)           \
	     (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \
	done

test : test-prep test-batch

clean :
	@rm -f '$(AUTOLOADS_FILE)' *.elc *~ */*.elc */*~ .DS_Store */.DS_Store *.bak */*.bak
