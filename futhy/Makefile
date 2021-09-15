default:
	stack build --force-dirty --ghc-options=-fforce-recomp
t: lib
	stack test

lib:
	cp lib/lmaplib.fut build/lmaplib.fut