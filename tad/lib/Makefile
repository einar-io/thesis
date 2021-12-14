default: clean
	clear
	futhark c lmaplib.fut

ex1: clean
	clear
	futhark c ex1.fut
	echo "" | ./ex1

clean:
	rm -rf ex1 ex1.c
	rm -rf lmaplib lmaplib.c
