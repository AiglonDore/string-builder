CC=ocamlc

CCX=ocamlopt

all : bytecode

bytecode : string_builder.ml
	$(CC) -o string_builder.out $<

exec : string_builder.ml
	$(CCX) -o string_builder.out $<

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.out