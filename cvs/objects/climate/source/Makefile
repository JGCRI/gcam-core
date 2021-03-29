#------------------------------------------------------------------------
# Makefile for objects/climate
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

SRCS = $(wildcard *.cpp)
OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.d)

ifeq ($(USE_HECTOR),1)
TARGS = $(OBJS) hector_dir
else
TARGS = $(OBJS)
endif

climate_dir: $(TARGS)

hector_dir:
	$(MAKE) -C ./hector/src -f makefile.standalone libhector.a
	cp -f ./hector/src/libhector.a ../../build/linux/

-include $(DEPS)

clean:
	rm *.o *.d
	$(MAKE) -C ./hector/src -f makefile.standalone clean
