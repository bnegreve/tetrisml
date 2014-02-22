tetris: tetris.ml
	/home/benjamin/local/ocaml/bin/ocamlc -o $@ unix.cma  -thread threads.cma graphics.cma $^ 
