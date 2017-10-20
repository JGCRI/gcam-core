#------------------------------------------------------------------------
# Makefile for objects/containers
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ../../build/linux/configure.gcam

OBJS       = batch_runner.o \
             dependency_finder.o \
             gdp.o \
             info.o \
             info_factory.o \
             mac_generator_scenario_runner.o \
             merge_runner.o \
             national_account.o \
             output_meta_data.o \
             region.o \
             region_cge.o \
             region_minicam.o \
             scenario.o \
             scenario_runner_factory.o \
             sector_cycle_breaker.o \
             single_scenario_runner.o \
             total_policy_cost_calculator.o \
             final_demand_activity.o \
             land_allocator_activity.o \
             resource_activity.o \
             sector_activity.o \
             market_dependency_finder.o \
             consumer_activity.o \
             world.o

containers_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
