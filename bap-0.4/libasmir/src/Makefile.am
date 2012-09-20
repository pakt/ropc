
RANLIB = @RANLIB@
lib_LIBRARIES = libasmir.a
#lib_LTLIBRARIES = libasmir.la
AM_CFLAGS = -I@VEX_DIR@/pub -I../include -Itraces -fPIC -DGET_OPERANDS
AM_CXXFLAGS = $(AM_CFLAGS)


libasmir_a_SOURCES = \
	./old_ir/stmt.cpp \
	./old_ir/stmt_c.cpp \
	./old_ir/exp.cpp \
	./old_ir/exp_c.cpp \
	asm_program.c \
	symbols.c \
	memory_data.cpp \
	irtoir_c.cpp \
	irtoir.cpp \
	irtoir-i386.cpp \
	irtoir-arm.cpp \
	vexir.c \
	vexmem.c \
	ocaml_helpers.c \
	./traces/readtrace.cpp \
	./traces/trace.cpp \
	./traces/trace_v40.cpp \
	./traces/trace_v41.cpp \
	./traces/trace_v50.cpp \
	./traces/reg_mapping.cpp \
        ./traces/pin_frame.h ./traces/pin_frame.cpp \
        ./traces/pin_trace.h ./traces/pin_trace.cpp
libasmir_a_LIBADD = @VEX_DIR@/libvex.a
