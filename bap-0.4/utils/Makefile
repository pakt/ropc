export BAP_DIR=$(CURDIR)/..

-include ../../utils-proj/Makefile

export BAP_OCAMLDIR=$(BAP_DIR)/ocaml
export BIGINT=$(BAP_DIR)/bigint-3.12/otherlibs/num
export BATT=$(BAP_DIR)/batteries-1.4.0/_build/src
export PCRE=$(BAP_DIR)/pcre-ocaml-release-6.2.2/lib
export OCAMLMAKEFILE=$(BAP_OCAMLDIR)/OCamlMakefile

# Stop annoying warnings about big_int duplication!
export OCAMLFIND_IGNORE_DUPS_IN=$(BAP_DIR)/bigint-3.12/otherlibs/num

# ocamlfind packages required
export PACKS = bigarray str ocamlgraph unix camomile
export LIBS = nums batteries bap pcre
export INCDIRS = $(BAP_OCAMLDIR) $(BIGINT) $(BATT) $(PCRE)

# Include sources from ../ocaml in the automatically generated dependencies.
export SOURCE_DIRS_IN = $(BAP_OCAMLDIR) $(BAP_PROJ_OCAMLDIR)

# When BAP lib changes, we need to recompile utilities
export RESULTDEPS = $(BAP_OCAMLDIR)/bap.a

# default subprojects to build
ifndef SUBPROJS
	export SUBPROJS = $(PROJUTILS) toil iltrans ileval topredicate formula get_functions split streamtrans
endif

define PROJ_toil
	RESULT=toil
	SOURCES = toil.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_toil

define PROJ_iltrans
	RESULT=iltrans
	SOURCES = iltrans.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_iltrans

define PROJ_ileval
	RESULT=ileval
	SOURCES = ileval.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_ileval

define PROJ_topredicate
	RESULT=topredicate
	SOURCES = topredicate.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_topredicate

define PROJ_formula
	RESULT=formula
	SOURCES = formula.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_formula

define PROJ_get_functions
	RESULT=get_functions
	SOURCES = get_functions.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_get_functions

define PROJ_split
	RESULT=split_disjunction
	SOURCES = split_disjunction.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_split

define PROJ_streamtrans
	RESULT=streamtrans
	SOURCES = streamtrans.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_streamtrans


# suffix for bytecode (which when we make all is debug code)
export BCSUFFIX = .dbg

all: nc dc

# Recursive make
tags:
	echo 'Making tags (not really)'

%:
	make -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@


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
