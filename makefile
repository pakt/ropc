BAP_DIR = ./bap-0.4

BAP_LIB_DIR = $(BAP_DIR)/ocaml/
LIBASMIR=$(BAP_DIR)/libasmir/src
BIGINT=$(BAP_DIR)/bigint-3.12/otherlibs/num
BATT=$(BAP_DIR)/batteries-1.4.0/_build/src
OUNIT=$(BAP_DIR)/ounit-1.1.0/_build/src/
PCRE=$(BAP_DIR)/pcre-ocaml-release-6.2.2/lib
INC=$(BAP_LIB_DIR) $(PCRE) $(LIBASMIR) $(BATT) $(OUNIT) $(BIGINT)
INC_PARAMS=$(foreach d, $(INC), -I $d)
 
PACKS = bigarray,str,ocamlgraph,unix,camomile,threads
BAP = $(BAP_LIB_DIR)/bap.cma

OBJS = common.cmo ast.cmo lexer.cmo parser.cmo main.cmo

GADGET_OBJS = common.cmo int_utils.cmo gdefs.cmo 
VERIFY_OBJS = common.cmo int_utils.cmo gdefs.cmo 

OPTS = 

all: ropc gadget
	
ropc: common.cmo lexer.cmo cdefs.cmo analysis.cmo main.cmo 
	ocamlfind ocamlc -package extlib -linkpkg -o ropc common.cmo ast.cmo lexer.cmo parser.cmo cdefs.cmo analysis.cmo main.cmo

analysis.cmo: ast.cmo analysis.ml
	ocamlc -c analysis.ml

cdefs.cmo: ast.cmo cdefs.ml
	ocamlc -c cdefs.ml

main.cmo: lexer.cmo parser.cmo analysis.cmo main.ml
	ocamlfind ocamlc -package extlib  -c $(OPTS) main.ml

gadget: gadget.ml test.o $(GADGET_OBJS) $(BAP)
	ocamlfind ocamlc -g -o gadget -package $(PACKS) -linkpkg -thread $(INC_PARAMS) pcre.cma $(LIBASMIR)/libasmir.a bap.cma $(GADGET_OBJS)  gadget.ml

verify: verify.ml $(VERIFY_OBJS) $(BAP) 
	ocamlfind ocamlc -g -o verify -package $(PACKS) -linkpkg -thread $(INC_PARAMS) pcre.cma bap.cma $(VERIFY_OBJS) verify.ml

dumper: dumper.ml $(VERIFY_OBJS) $(BAP)
	ocamlfind ocamlc -g -o dumper -package $(PACKS) -linkpkg -thread $(INC_PARAMS) pcre.cma bap.cma $(VERIFY_OBJS) dumper.ml

common.cmo: common.ml
	ocamlc -c $(OPTS) $(INC_PARAMS) common.ml

int_utils.cmo: int_utils.ml
	ocamlc -c $(OPTS) $(INC_PARAMS) int_utils.ml

gdefs.cmo: gdefs.ml
	ocamlc -c $(OPTS) $(INC_PARAMS) gdefs.ml

test.o: test.asm
	nasm -felf test.asm -o test.o

lexer.cmo: parser.cmo lexer.mll 
	ocamllex lexer.mll
	ocamlc -c $(OPTS) lexer.ml

ast.cmo: ast.ml
	ocamlc -c $(OPTS) ast.ml

parser.cmo: ast.cmo parser.mly
	ocamlyacc parser.mly
	ocamlc -c $(OPTS) parser.mli
	ocamlc -c $(OPTS) parser.ml 

asm: test.asm test.c
	nasm -felf test.asm -o asm.o
	gcc asm.o test.c 

vg: asm 
	./gadget a.out candidates.bin
	./verify candidates.bin vg.bin
	./ropc examples-ropl/fib.ropl vg.bin

test-all: dumper gadget verify ropc vg 
	./a.out compiled.bin

clean:
	rm -f *.o *.cmo *.cmi gadget verify a.out dumper ropc *.formula.txt parser.ml lexer.ml parser.mli lexer.mli
	

