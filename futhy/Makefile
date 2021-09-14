default:
	stack build --force-dirty --ghc-options=-fforce-recomp
t:
	rm build/*
	cp lib/lmaplib.fut build/lmaplib.fut
	stack test
