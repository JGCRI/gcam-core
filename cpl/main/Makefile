#------------------------------------------------------------------------
# Makefile for main
# Kate Calvin
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../../cvs/objects
include $(PATHOFFSET)/build/linux/config.system
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = main.o

-include $(DEPS)

#cpl code is in a different directory
cpl_dir:
	$(MAKE) -C ../source $@

main_dir: ${OBJS} iesm.exe 

iesm.exe : main.o cpl_dir
	@echo main_dir: LIB:  $(LIB)
	$(RANLIB) ${PATHOFFSET}/build/linux/libgcam.a
	$(CXX) -o iesm.exe $(LDFLAGS) main.o ../source/*.o -lgcam $(LIB) 

clean:
	rm *.o *.d
