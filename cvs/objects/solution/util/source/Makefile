#------------------------------------------------------------------------
# Makefile for objects/solution/util
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../../..
include ${PATHOFFSET}/build/linux/configure.gcam

SRCS    = $(wildcard *.cpp)
OBJS       = $(SRCS:.cpp=.o)

solution_util_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
