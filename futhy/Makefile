default:
	stack build --force-dirty --ghc-options=-fforce-recomp
t:
	rm -f build/lmaplib.fut
	cp lib/lmaplib.fut build/lmaplib.fut
	stack test 2> /dev/null
