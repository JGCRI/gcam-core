/* usage example:
   char buf[1024];
   FILE *f;
   filename = find_file(buf, "input.txt",1024);
   f = fopen(filename,"r");
*/ 
char *find_file(char *full_file_name, const char *env, const char *part_file_name, int nbuf);

