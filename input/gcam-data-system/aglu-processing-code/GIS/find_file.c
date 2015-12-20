#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "find_file.h"

/* usage example:
   char buf[1024];
   FILE *f;
   filename = find_file(buf, "input.txt",1024);
   f = fopen(filename,"r");
*/
char *find_file(char *full_file_name, const char *env, const char *part_file_name, int nbuf)
{
  int fnlen    = strlen(part_file_name);
  int prmaxlen = nbuf - fnlen - 2;              /* leave room for terminator and separator */
  char *prefix = getenv(env);
  int prlen    = prefix ? strlen(prefix) : 1;

  if(prlen > prmaxlen) {
    fprintf(stderr,"Buffer length too short to hold prefix and filename.\n");
    if(prefix)
      fprintf(stderr,"prefix:  %s/GIS/\n", prefix);
    else
      fprintf(stderr,"prefix:  ./GIS/\n");
    fprintf(stderr,"filename:  %s\n",part_file_name);
    fprintf(stderr,"buffer length: %d\n", nbuf);
    exit(3);
  }

  if(prefix)
    strncpy(full_file_name, prefix, prmaxlen+1);
  else
    strncpy(full_file_name,".",2);

  strncat(full_file_name, "/", 1);
  strncat(full_file_name, part_file_name, fnlen);

  return full_file_name; 
}
