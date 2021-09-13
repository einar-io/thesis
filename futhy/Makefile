default:
	stack build --force-dirty --ghc-options=-fforce-recomp
t: clean
	cp lib/lmaplib.fut build/lmaplib.fut
	stack test --force-dirty --ghc-options=-fforce-recomp

clean:
	rm build/*