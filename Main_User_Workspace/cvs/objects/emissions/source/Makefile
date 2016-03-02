#------------------------------------------------------------------------
# Makefile for objects/emissions
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = nonco2_emissions.o \
             aghg.o \
             co2_emissions.o \
             emissions_driver_factory.o \
             emissions_summer.o \
             ghg_factory.o \
             input_driver.o \
             input_output_driver.o \
             luc_emissions_summer.o \
             output_driver.o \
             emissions_control_factory.o \
             aemissions_control.o \
             mac_control.o \
             gdp_control.o \
             readin_control.o

emissions_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o
