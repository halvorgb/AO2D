all: Main.hs
	ghc --make Main.hs -o out

.PHONY: clean

clean:
	find ./ -type f -name "out" -delete
	find ./ -type f -name "*.o" -delete
	find ./ -type f -name "*.hi" -delete
