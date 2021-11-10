.PHONY: default build bench test tags t e f b

default:
	stack build --force-dirty #--ghc-options="-fforce-recomp -fprof-auto -fprof-cafs"

build:
	stack build

bench:
	stack bench

test:
	rm -f build/lmaplib.fut
	cp lib/lmaplib.fut build/lmaplib.fut
	stack test

tags:
	hasktags .

t: test

e: tags
	stack build
	stack test 2> /dev/null

f: build test

b: bench

