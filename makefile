line: line.hs
	ghc -dynamic -O2 line

clean:
	rm *.hi *.o
