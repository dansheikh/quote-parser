OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

check:
	ocamlfind query core async
clean:
	$(OCB) -clean
byte:
	$(OCB) main.byte
native:
	$(OCB) main.native

.PHONY: check clean byte native

