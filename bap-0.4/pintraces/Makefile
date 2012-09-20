##
## This is a sample makefile for building Pin tools outside
## of the Pin environment.  This makefile is suitable for
## building with the Pin kit, not a Pin source development tree.
##
## To build the tool, execute the make command:
##
##      make
## or
##      make PIN_HOME=<top-level directory where Pin was installed>
##
## After building your tool, you would invoke Pin like this:
## 
##      $PIN_HOME/pin -t MyPinTool -- /bin/ls
##
##############################################################
#
# User-specific configuration
#
##############################################################

#
# 1. Change PIN_HOME to point to the top-level directory where
#    Pin was installed. This can also be set on the command line,
#    or as an environment variable.
#
PIN_HOME ?= ../pin/


# We only support ia32
TARGET=ia32

##############################################################
#
# set up and include *.config files
#
##############################################################

PIN_KIT=$(PIN_HOME)
KIT=1
TESTAPP=$(OBJDIR)cp-pin.exe
DEBUG=0

TARGET_COMPILER?=gnu
ifdef OS
    ifeq (${OS},Windows_NT)
        TARGET_COMPILER=ms
    endif
endif

ifeq ($(TARGET_COMPILER),gnu)
    include $(PIN_HOME)/source/tools/makefile.gnu.config
    CXXFLAGS ?= -Wall -Werror -Wno-unknown-pragmas $(DBG) $(OPT)
    PIN=$(PIN_HOME)/pin
	RM = rm -rf
endif

ifeq ($(TARGET_COMPILER),ms)
    include $(PIN_HOME)/source/tools/makefile.ms.config
    CXXFLAGS +=  -Iddk-inc 
    PIN=$(PIN_HOME)/pin.bat
	RM = del /Q
endif

##############################################################
#
# Tools - you may wish to add your tool name to TOOL_ROOTS
#
##############################################################


TOOL_ROOTS = gentrace 

TOOLS = $(TOOL_ROOTS:%=$(OBJDIR)%$(PINTOOL_SUFFIX))

OBJS = $(OBJDIR)pin_taint.o $(OBJDIR)winsyscalls.o $(OBJDIR)pivot.o

CXXFLAGS += -I../libasmir/include -I../libasmir/src/traces

TRACEDIR = ../libasmir/src/traces

##############################################################
#
# build rules
#
##############################################################

all: tools reader
tools: $(OBJDIR) $(TOOLS)
test: $(OBJDIR) $(TOOL_ROOTS:%=%.test)

gentrace.test: $(OBJDIR)cp-pin.exe
	$(MAKE) -k -C MyPinTool PIN_HOME=$(PIN_HOME)

$(OBJDIR)cp-pin.exe:
	$(CXX) $(PIN_HOME)/source/tools/Tests/cp-pin.cpp $(APP_CXXFLAGS) -o $(OBJDIR)cp-pin.exe

$(OBJDIR):
	-mkdir "$(OBJDIR)"

$(OBJDIR)gentrace.o : gentrace.cpp $(OBJDIR)pivot.o $(OBJDIR)pin_taint.o $(OBJDIR)pin_frame.o $(OBJDIR)pin_trace.o $(OBJDIR)winsyscalls.o
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

$(OBJDIR)pivot.o : pivot.cpp
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

$(OBJDIR)pin_frame.o : $(TRACEDIR)/pin_frame.cpp
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

$(OBJDIR)pin_trace.o : $(TRACEDIR)/pin_trace.cpp
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

$(OBJDIR)pin_taint.o : $(TRACEDIR)/pin_taint.cpp
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

$(OBJDIR)winsyscalls.o : $(TRACEDIR)/winsyscalls.cpp
	$(CXX) -c $(CXXFLAGS) $(PIN_CXXFLAGS) ${OUTOPT}$@ $<

reader: readtrace.cpp $(OBJDIR)pin_frame.o $(OBJDIR)pin_trace.o
	$(CXX) -o $@ $^ $(CXXFLAGS)

pivottest: $(OBJDIR)pivot.o pivottest.cpp
	$(CXX) -o $@ $^ $(CXXFLAGS) $(PIN_CXXFLAGS)

$(TOOLS): $(PIN_LIBNAMES)

$(TOOLS): %$(PINTOOL_SUFFIX) : %.o
	${PIN_LD} $(PIN_LDFLAGS) $(LINK_DEBUG) ${LINK_OUT}$@ $(OBJS) $< ${PIN_LPATHS} $(PIN_LIBS) $(DBG)


## cleaning
clean:
	-$(RM) "$(OBJDIR)" *.out *.tested *.failed makefile.copy reader

tags: 

.PHONY: tags
