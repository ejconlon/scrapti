include Makefile.base

.PHONY: exe
exe:
	stack build --fast --test --no-run-tests --exec scrapti-exe
