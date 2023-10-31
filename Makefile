.phony : binary test

binary:
	@dune build compilou.exe

byte:
	@dune build compilou.bc

clean:
	@dune clean
	@rm -f *.exe

test:
	./test.sh

test_macos:
	./test_macos.sh