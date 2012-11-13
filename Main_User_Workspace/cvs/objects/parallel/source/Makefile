#------------------------------------------------------------------------
# Makefile for objects/containers
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ../../build/linux/configure.gcam

OBJS       = gcam_parallel.o

parallel_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o
