#------------------------------------------------------------------------
# Makefile for objects/land_allocator_finder
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = aland_allocator_item.o \
             forest_land_leaf.o \
             land_leaf.o \
             land_node.o \
             land_use_history.o \
             tree_land_allocator.o \
             unmanaged_land_leaf.o

land_allocator_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o
