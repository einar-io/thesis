.PHONY: default build bench test tags t e f b

default:
	time stack build --force-dirty

build:
	time stack build

bench:
	rm -f build/lmaplib.fut
	cp lib/lmaplib.fut build/lmaplib.fut
	time stack bench

test:
	rm -f build/lmaplib.fut
	cp lib/lmaplib.fut build/lmaplib.fut
	time stack test

tags:
	hasktags .

t: test

e: tags
	stack build
	stack test 2> /dev/null

f: build test

b: bench

