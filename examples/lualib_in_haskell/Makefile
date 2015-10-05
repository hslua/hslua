ALL: libarith.so lualibhelper.so

libarith.so: LibArith.hs
	ghc $^ -o $@ -shared -fPIC -dynamic -lHSrts-ghc7.10.1

lualibhelper.so: libarith.so libarithhelper.c
	ghc libarithhelper.c -no-hs-main -o $@ -shared -fPIC -dynamic -lHSrts-ghc7.10.1 -L. -larith -I../../lua-5.1.5

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.so
	rm -f *_stub.h
