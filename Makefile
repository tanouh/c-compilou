.phony : build run test

binary:
	@dune build compilou.exe

byte:
	@dune build compilou.bc

clean:
	@dune clean
	@rm -f ./main.bc
	@rm tests/*.s

test:
	./test.sh

test_macos:
	./test_macos.sh

debug: byte
	@rm -f bin/main.bc
	@cp _build/default/bin/main.bc .
	@ocamldebug main.bc

run: binary
	@dune exec bin/main.exe -- $(ARGS)

