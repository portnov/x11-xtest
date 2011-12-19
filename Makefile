GHC=ghc -lXtst --make

all: test

test: test.hs Graphics/X11/*.hs
	$(GHC) -o $@ $<

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	rm test
