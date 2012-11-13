#------------------------------------------------------------------------
# Makefile for objects/main
# William.Gustafson@pnl.gov; 8-Sep-2008
#------------------------------------------------------------------------

#PATHOFFSET = path to objects directory
PATHOFFSET = ../..
include ${PATHOFFSET}/build/linux/configure.gcam

OBJS       = main.o

main_dir: ${OBJS} gcam.exe

-include $(DEPS)

gcam.exe : main.o
	$(RANLIB) ${PATHOFFSET}/build/linux/libgcam.a
	$(CXX) -o gcam.exe $(LDFLAGS) main.o -lgcam $(LIB) 

clean:
	rm *.o
