all: src/Main.hs
	cd src; \
	 ghc Main.hs -o ../bin/AO2D -O3 -Wall -fwarn-unused-imports

.PHONY: clean

clean:
	find ./ -type f -name "*.o" -delete
	find ./ -type f -name "*.hi" -delete
