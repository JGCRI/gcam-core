#include <stdio.h>
#include <string.h>
#include "find_file.h"


int main(int argc, char *argv[])
{
  char buf[1024];
  char *instr;
  char *fn;
  int ntot;

  if(argc > 1)
    instr = argv[1];
  else
    instr = argv[0];
  
  fn = find_file(buf, instr, 1024);

  printf("filename: %s\n",fn);

  ntot = strlen(fn);

  /* this should (barely) succeed */
  fn = find_file(buf, instr, ntot+1);

  printf("filename: %s\n", fn);

  /* this one should fail */
  fn = find_file(buf, instr, ntot);
  printf("filename:  %s\n", fn);

  return 0;
}
