#------------------------------------------------------------------------
# Makefile for objects/consumers
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = consumer.o \
             gcam_consumer.o 

consumers_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
