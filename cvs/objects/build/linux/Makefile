# Top level Makefile for MiniCAM
# William.Gustafson@pnl.gov; 9-Sep-2008

## pointer back to build directory for subsidiary makefiles
export BUILDPATH = $(PWD)
## Load system-specific paths
include ./config.system
## Load configuration
include ./configure.gcam

OBJDIR = ./objs

all: gcam

gcam-prof: export PROF = -pg
gcam-prof: libgcam.a main_dir

gcam: libgcam.a main_dir

libgcam.a: dirs
	$(AR) libgcam.a $(OBJDIR)/*.o

dirs : containers_dir demographics_dir emissions_dir marketplace_dir resources_dir sectors_dir solution_solvers_dir solution_util_dir technologies_dir util_base_dir util_logger_dir util_curves_dir consumers_dir reporting_dir climate_dir functions_dir target_finder_dir land_allocator_dir ccarbon_model_dir policy_dir parallel_dir

## special case patterns first
# util has subdirs
util_%_dir:
	$(MAKE) -C ../../util/$*/source $@
	ln -sf $(PWD)/../../util/$*/source/*.o $(OBJDIR)

#solution also has subdirs
solution_%_dir:
	$(MAKE) -C ../../solution/$*/source $@
	ln -sf $(PWD)/../../solution/$*/source/*.o $(OBJDIR)

#general pattern
%_dir:
	$(MAKE) -C ../../$*/source $@
	ln -sf $(PWD)/../../$*/source/*.o $(OBJDIR)

# main has additional instructions and doesn't do the softlink
main_dir : libgcam.a
	@ echo '----------------------------------------------------------------'
	rm -f ../../main/source/gcam.exe
	$(MAKE) -C ../../main/source  BUILDPATH=$(BUILDPATH) main_dir 
	cp ../../main/source/gcam.exe ../../../../exe/
	@echo BUILD COMPLETED
	@date


# target for debugging configure.gcam 
varchk:
	@echo GCAMLIB_HOME:  $(GCAMLIB_HOME)
	@echo INCLUDE: $(INCLUDE)
	@echo BOOSTINC: $(BOOSTINC)
	@echo BOOST_LIB: $(BOOST_LIB)
	@echo TBB_INCLUDE: $(TBB_INCLUDE)
	@echo BOOSTBIND: $(BOOSTBIND)
	@echo DBXMLINC: $(DBXMLINC)
	@echo PATHOFFSET: $(PATHOFFSET)
	@echo HOME:  $(HOME)
	@echo LAPACKCF: $(LAPACKCF)
	@echo
	@echo LAPACKINC: $(LAPACKINC)
	@echo LIBDIR: $(LIBDIR)
	@echo LAPACKLD: $(LAPACKLD)
	@echo LDFLAGS: $(LDFLAGS)
	@echo LIB: $(LIB)
	@echo HECTOR_LIB: $(HECTOR_LIB)
	@echo
	@echo USE_LAPACK: $(USE_LAPACK)
	@echo MLIB_CFLAGS: $(MLIB_CFLAGS)
	@echo MKL_CFLAGS: $(MKL_CFLAGS)
	@echo MKL_LIB: $(MKL_LIB)
	@echo MKL_LDFLAGS: $(MKL_LDFLAGS)
	@echo MKL_RPATH: $(MKL_RPATH)

clean :
	-$(RM) libgcam.a
	-$(RM) objs/*.o
	-$(MAKE) -C ../../containers/source  clean 
	-$(MAKE) -C ../../demographics/source  clean 
	-$(MAKE) -C ../../emissions/source  clean 
	-$(MAKE) -C ../../marketplace/source  clean 
	-$(MAKE) -C ../../resources/source  clean 
	-$(MAKE) -C ../../sectors/source  clean 
	-$(MAKE) -C ../../solution/solvers/source  clean 
	-$(MAKE) -C ../../solution/util/source  clean 
	-$(MAKE) -C ../../technologies/source  clean 
	-$(MAKE) -C ../../util/base/source  clean 
	-$(MAKE) -C ../../util/logger/source  clean 
	-$(MAKE) -C ../../util/curves/source  clean 
	-$(MAKE) -C ../../consumers/source  clean 
	-$(MAKE) -C ../../investment/source  clean 
	-$(MAKE) -C ../../reporting/source  clean 
	-$(MAKE) -C ../../climate/source  clean 
	-$(MAKE) -C ../../functions/source  clean 
	-$(MAKE) -C ../../target_finder/source  clean 
	-$(MAKE) -C ../../land_allocator/source  clean 
	-$(MAKE) -C ../../ccarbon_model/source  clean 
	-$(MAKE) -C ../../policy/source  clean 
	-$(MAKE) -C ../../parallel/source  clean 
	-$(MAKE) -C ../../main/source  clean 
