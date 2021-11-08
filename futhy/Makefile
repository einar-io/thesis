default:
	stack build --force-dirty

t: test

test:
	rm -f build/lmaplib.fut
	cp lib/lmaplib.fut build/lmaplib.fut
	stack test

.PHONY: tags
tags:
	hasktags .

e: tags
	stack build
	stack test 2> /dev/null

f:
	stack build
	stack test

b: bench

bench:
	stack bench
