#------------------------------------------------------------------------
# Makefile for objects/containers
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ../../build/linux/configure.gcam

SRCS	= $(wildcard *.cpp)
OBJS	= $(SRCS:.cpp=.o)

containers_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
