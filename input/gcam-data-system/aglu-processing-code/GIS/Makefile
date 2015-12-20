CC	= gcc
CFLAGS	= -g -O
OBJS	= find_file.o

all: Nfert.exe SAGE15_HYDE_Area.exe  SAGE5_HYDE_Area.exe  SAGE_Area.exe  Soil_Carbon.exe

%.exe: %.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

## Don't build test codes with a .exe extension; it will cause the aglu data processing
## scripts to try to execute them.
find_file_test: find_file_test.c find_file.o
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm *.exe
	rm *.o
