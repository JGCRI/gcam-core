#------------------------------------------------------------------------
# Makefile for objects/reporting
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = batch_csv_outputter.o \
             graph_printer.o \
             land_allocator_printer.o \
             storage_table.o \
             energy_balance_table.o \
             xml_db_outputter.o

reporting_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
