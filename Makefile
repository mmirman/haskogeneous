test : clean
	ghc RPCTest.hs -threaded -o test
	rm -f *.hi *.o *~

run : test
	./test

clean :
	rm -f *.hi *.o *~ test