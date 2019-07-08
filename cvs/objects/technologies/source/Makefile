#------------------------------------------------------------------------
# Makefile for objects/technologies
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

SRCS	= $(wildcard *.cpp)
OBJS	= $(SRCS:.cpp=.o)

technologies_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
