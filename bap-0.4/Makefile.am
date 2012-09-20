# Owned and copyright BitBlaze, 2007. All rights reserved.
# Do not copy, disclose, or distribute without explicit written
# permission.

#include @top_srcdir@/config/recursive.mk

if TESTS_OPT
TESTPOSTDIRS=tests stpwrap
endif

SUBDIRS=bigint-3.12 batteries-1.4.0 ounit-1.1.0 pcre-ocaml-release-6.2.2 VEX libasmir ocaml utils $(TESTPOSTDIRS)


# pintraces removed temporarily
# stp/ocaml ocaml utils doc

test: all
	$(MAKE) -C tests test

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

#regression:
#	cd $(top_srcdir)/../regression/ocaml;./test.sh

all-local: tags
