bin_PROGRAMS = \
	print-ir \
	lifttrace

# these executables should depend on libasmir, since they link it in
# statically.
#print-ir_DEPENDENCIES = ../src/libasmir.a
#lifttrace_DEPENDENCIES = ../src/libasmir.a

print_ir_SOURCES = print-ir.cpp ir_printer.cpp
lifttrace_SOURCES = lifttrace.cpp
LDADD =  @ASMIR_DIR@/src/libasmir.a @VEX_DIR@/libvex.a -lbfd -ldl -liberty -lopcodes
# AM_LDFLAGS = -L../src -L@VEX_DIR@ -L@BINUTILS_DIR@
AM_LDFLAGS = -L@BINUTILS_DIR@
AM_CXXFLAGS = \
              -I../include \
              -I../src/traces \
              -I@VEX_DIR@/pub -I@BINUTILS_DIR@

