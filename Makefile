.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -l stopwatch.el \
		-l test/test-model.el \
		-f ert-run-tests-batch-and-exit

package-for-local-installation: *.el
	@version=`grep -o "Version: .*" stopwatch.el | cut -c 10-`; \
	dir=stopwatch-$$version; \
	mkdir -p "$$dir"; \
	cp $$(find . -name \*.el) stopwatch-$$version; \
	echo "(define-package \"stopwatch\" \"$$version\" \
	\"time yo'self\")" \
	> "$$dir"/stopwatch-pkg.el; \
	tar cvf stopwatch-$$version.tar "$$dir"

clean:
	@rm -rfv stopwatch-*/ stopwatch-*.tar
