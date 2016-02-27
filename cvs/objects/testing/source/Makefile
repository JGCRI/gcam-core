#------------------------------------------------------------------------
# Makefile for objects/testing
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

SRCS	= $(wildcard *.cpp)
OBJS       = $(SRCS:.cpp=.o)

testing_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
