all: 
	cc -lX11 -lXtst cursor/dragdrop.c -o cursor/dragdrop; ocamlbuild echess.native