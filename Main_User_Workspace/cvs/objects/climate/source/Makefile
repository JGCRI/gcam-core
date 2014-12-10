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
## repository.  To use hector in GCAM, clone (i.e., check out ) the
## hector repository into a subdirectory called hector under this
## source directory.  Making the install_hector target should do this
## automatically as well as switching to the official gcam branch.
##
install_hector:
	git clone https://github.com/JGCRI/hector.git hector
	cd hector && git checkout gcam-devel

hector_dir:
	$(MAKE) -C ./hector/source libhector.a
	cp ./hector/source/libhector.a ../../build/linux/

-include $(DEPS)

clean:
	rm *.o
	$(MAKE) -C ./hector/source clean
