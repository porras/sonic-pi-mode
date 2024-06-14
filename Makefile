CASK ?= cask

default: build

build: # test
	${CASK} build

# test:
# 	${CASK} exec ert-runner --quiet

deps:
	${CASK} install

clean-elc:
	${CASK} clean-elc
	rm *-autoloads.el

clean: clean-elc
	rm -rf .cask

check: clean-elc elisp-lint package-lint

elisp-lint:
	${CASK} emacs -Q --batch \
	--eval "(require 'elisp-lint)" \
	-f elisp-lint-files-batch *.el --no-package-lint

package-lint:
	${CASK} emacs -Q --batch \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit sonic-pi-mode.el

# .PHONY: test
