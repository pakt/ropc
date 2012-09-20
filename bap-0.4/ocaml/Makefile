# $Id: Makefile 5241 2011-09-21 23:20:19Z edmcman $

export BAP_DIR=$(CURDIR)/..

export OCAMLMAKEFILE=OCamlMakefile
export LIBASMIR=../libasmir
export BIGINT=$(BAP_DIR)/bigint-3.12/otherlibs/num
export BATT=$(BAP_DIR)/batteries-1.4.0/_build/src
export OUNIT=$(BAP_DIR)/ounit-1.1.0/_build/src/
export PCRE=$(BAP_DIR)/pcre-ocaml-release-6.2.2/lib

include $(LIBASMIR)/makefile.inc
-include ../../ocaml-proj/Makefile

# Stop annoying warnings about big_int duplication!
export OCAMLFIND_IGNORE_DUPS_IN=$(BAP_DIR)/bigint-3.12/otherlibs/num

#export INCDIRS += ../stp ../stp/ocaml
#export EXTLIBDIRS += ../stp ../stp/ocaml
#export LIBS += stpvc

#export LIB_PACK_NAME = bap

# common sources
export SOURCES = \
	debug.mli debug.ml \
	BatListFull.ml \
	util.mli util.ml \
	big_int_convenience.ml \
	dominator.ml \
	graphDataflow.mli graphDataflow.ml \
	type.ml type.mli \
	var.mli var.ml \
	arithmetic.ml \
	ast.ml \
	ssa.ml \
	pp.ml \
	typecheck.ml \
	ast_convenience.ml \
	cfg.mli cfg.ml \
	reachable.mli \
	reachable.ml \
	prune_unreachable.mli prune_unreachable.ml \
	cfg_pp.ml \
	cfg_ast.mli cfg_ast.ml \
	ssa_visitor.mli ssa_visitor.ml \
	cfg_ssa.mli cfg_ssa.ml \
	ast_visitor.mli ast_visitor.ml \
	debug_snippets.ml \
	sccvn.mli sccvn.ml \
	deadcode.mli deadcode.ml \
	ssa_simp_misc.ml \
	ssa_simp.ml \
	formulap.ml \
	stp.ml \
	smtlib1.ml \
	smtlib2.ml \
	libbfd.idl \
	libasmir.idl \
	disasm_i386.ml \
	disasm.ml \
	memory2array.ml \
	gcl.mli gcl.ml \
	wp.ml \
	stp_grammar.mly stp_lexer.mll \
	cvc3_grammar.mly cvc3_lexer.mll \
	yices_grammar.mly yices_lexer.mll \
	smtexec.mli smtexec.ml \
	asmirconsts.ml asmir.mli asmir.ml \
	grammar_scope.ml \
	grammar.mly lexer.mll \
	parser.ml \
	input.mli input.ml \
	depgraphs.ml \
	to_c.ml \
	vsa.ml \
	eval.ml \
	symbeval.ml symbeval_search.ml \
	ast_slice.ml \
	ssa_slice.ml \
	structural_analysis.ml \
	unroll.ml \
	hacks.ml \
	coalesce.ml \
	template.ml \
	traces.ml \
	graphDataflowW.ml \
	utils_common.ml \
	test_common.ml \
	$(PROJSOURCES) \

# Fix the interpreter to work with big ints
#	interpreter.ml \


RESULT = bap
DOC_FILES := $(filter %.mli, $(SOURCES)) $(filter %.ml, $(SOURCES))



# ocamlfind packages required
export PACKS = bigarray str ocamlgraph unix camomile threads
# ocamlfind packages for ocamldoc
# don't include packages in /usr/lib/ocaml, this will cause ocamldoc
# to complain about conflicts with our local big_int
export DOCPACKS = ocamlgraph
export YFLAGS = -v


export CLIBS = asmir vex opcodes bfd iberty stdc++ 
export CFLAGS = -g
export LDFLAGS = $(LIBASMIR_LDFLAGS)
export INCDIRS = $(BIGINT) $(BATT) $(OUNIT) $(PCRE)
export LIBS = nums batteries oUnit pcre
export CLIBFLAGS= $(LDFLAGS)
export RESULTDEPS = $(LIBASMIR)/src/libasmir.a $(VEX_DIR)/libvex.a
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLLDFLAGS = -thread # just for make top
OCAMLFLAGS = -thread -warn-error Aelz # -for-pack Bap #-w A
# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

all: dcl ncl #top
	@echo $(PROJSOURCES)

bap_lib_doc: all htdoc

docall: all htdoc ladoc psdoc pdfdoc

tags: TAGS

TAGS: $(SOURCES)
	-otags $^

runtop:	top
	./bap.top -I $(BIGINT)

# You may also tell "make" at the command-line what kind of target to
# produce (e.g. "make nc").  Here all the possibilities with shortcuts
# between parenthesis:
# 
#    * byte-code                     (bc)
#    * byte-code-nolink              (bcnl)   - no linking stage
#    * byte-code-library             (bcl)
#    * native-code                   (nc)
#    * native-code-nolink            (ncnl)   - no linking stage
#    * native-code-library           (ncl)
#    * debug-code                    (dc)
#    * debug-code-nolink             (dcnl)   - no linking stage
#    * debug-code-library            (dcl)
#    * profiling-byte-code           (pbc)
#    * profiling-byte-code-library   (pbcl)
#    * profiling-native-code         (pnc)
#    * profiling-native-code-library (pncl)
#    * byte-code-dll                 (bcd)
#    * native-code-dll               (ncd)
#    * pack-byte-code              (pabc)
#    * pack-native-code            (panc)
#    * toplevel interpreter          (top)
#    * subprjs



include $(OCAMLMAKEFILE)

bap.dot: $(DOC_FILES)
	 ocamlfind ocamldoc -I /usr/lib/ocaml/3.10.2/ -package str,ocamlgraph,extlib,unix,sqlite3 -dot -o bap.dot -m avlsoper $(DOC_FILES)
