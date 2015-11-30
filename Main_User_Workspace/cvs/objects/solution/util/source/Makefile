#------------------------------------------------------------------------
# Makefile for objects/solution/util
# William.Gustafson@pnl.gov; 5-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = calc_counter.o \
             all_solution_info_filter.o \
             and_solution_info_filter.o \
             market_name_solution_info_filter.o \
             market_type_solution_info_filter.o \
             has_market_flag_solution_info_filter.o \
             not_solution_info_filter.o \
             or_solution_info_filter.o \
             solution_info.o \
             solution_info_filter_factory.o \
             solution_info_param_parser.o \
             solution_info_set.o \
             solvable_nr_solution_info_filter.o \
             solvable_solution_info_filter.o \
             unsolved_solution_info_filter.o \
             solver_library.o \
             price_greater_than_solution_info_filter.o \
             price_less_than_solution_info_filter.o \
			 jacobian-precondition.o \
			 svd_invert_solve.o \
             edfun.o 

solution_util_dir: ${OBJS}

-include $(DEPS)

clean:
	rm *.o *.d
