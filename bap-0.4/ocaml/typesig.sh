#!/bin/sh
ocamlfind ocamlc -package str,ocamlgraph,extlib,unix,sqlite3 -c -i -for-pack Bap -for-pack Bap $1
