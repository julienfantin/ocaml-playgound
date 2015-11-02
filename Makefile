CC=ocamlfind ocamlopt
BINARY=http
PKGS=lwt,cohttp,cohttp.lwt
SRCS=./src/http.ml

all:
	$(CC) -o $(BINARY) -linkpkg -package $(PKGS) $(SRCS)
