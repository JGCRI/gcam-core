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

##
## The hector climate model lives in its own self-contained
## repository.  To use hector in GCAM, unpack the hector distribution
## into a subdirectory called hector under this source directory.
## Making the install_hector target should do this automatically.
##
install_hector:
	-rm v1.1.2.tar.gz
	-rm hector
	wget https://github.com/JGCRI/hector/archive/v1.1.2.tar.gz
	tar -zxf v1.1.2.tar.gz
	ln -sf hector-1.1.2 hector

hector_dir:
	$(MAKE) -C ./hector/source libhector.a
	cp ./hector/source/libhector.a ../../build/linux/

-include $(DEPS)

clean:
	rm *.o *.d
	$(MAKE) -C ./hector/source clean
