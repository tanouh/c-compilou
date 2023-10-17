.phony : install build run test

binary:
	@dune build bin/main.exe

byte:
	@dune build bin/main.bc

clean:
	@dune clean
	@rm -f ./main.bc

test:
	@dune runtest

debug: byte
	@rm -f bin/main.bc
	@cp _build/default/bin/main.bc .
	@ocamldebug main.bc

run: binary
	@dune exec bin/main.exe -- $(ARGS)

