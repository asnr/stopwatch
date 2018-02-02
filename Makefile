.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -l stopwatch.el \
		-l test/test-model.el \
		-f ert-run-tests-batch-and-exit
