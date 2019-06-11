.PHONY: clean

build: sudoku.exe

sudoku.exe: main.hs
	ghc -o sudoku.exe main.hs

clean:
	rm *.hi *.o *.exe
