libffi-HsInterface.so: HsInterface.o Board.o Solver.o wrapper.o
	ghc -o $@ -shared -dynamic -fPIC $^ -lHSrts

HsInterface_stub.h Board.o Solver.o HsInterface.o: Board.hs Solver.hs HsInterface.hs
	ghc -c -dynamic -fPIC Board.hs Solver.hs HsInterface.hs

wrapper.o: wrapper.c
	ghc -c -dynamic -fPIC wrapper.c

clean:
	rm -f *.hi *.o *_stub.[ch]

clean-all:
	rm -f *.hi *.o *_stub.[ch] *.so