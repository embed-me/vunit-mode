.PHONY: all build tests clean

TEST_DIR=tests

EMACS=emacs
EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
EMACS_LOAD_DEPENDENCIES=(setq package-enable-at-startup nil)      \
			(defvar package-load-list '((lv t)        \
						    (hydra t)     \
						    (el-mock t))) \
			(package-initialize)

all : build tests

build :
	$(EMACS) $(EMACS_BATCH)                      \
		--eval "(progn                       \
			  $(EMACS_LOAD_DEPENDENCIES)           \
			  (setq byte-compile-error-on-warn t)  \
			  (batch-byte-compile))" *.el

tests :
	@cd '$(TEST_DIR)' &&                         \
	(for test in *-tests.el; do                  \
		$(EMACS) $(EMACS_BATCH) -L . -L ..   \
		--eval "(progn                       \
			  $(EMACS_LOAD_DEPENDENCIES))"        \
			  -l cl                               \
			  -l "$$test"                         \
			  -f "ert-run-tests-batch-and-exit";  \
	done)

clean :
	@rm -f *.elc
