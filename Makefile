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

clean: clean-elc
	rm -rf .cask

check: clean-elc
	${CASK} emacs -Q --batch \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit sonic-pi-mode.el

# .PHONY: test
