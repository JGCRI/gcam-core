#------------------------------------------------------------------------
# Makefile for objects/ccarbon_model
# William.Gustafson@pnl.gov; 8-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = acarbon_flow.o \
             acarbon_stock.o \
             box_flow.o \
             carbon_box.o \
             carbon_box_model.o \
             carbon_flow_factory.o \
             carbon_model_utils.o \
             carbon_stock.o \
             environmental_info.o \
             luc_carbon_summer.o \
             luc_carbon_summer_stock.o \
             luc_carbon_summer_sub_container.o \
             luc_flow_in_to_box.o \
             luc_flow_in_to_box_model.o \
             luc_flow_out.o \
             npp.o

ccarbon_model_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o
