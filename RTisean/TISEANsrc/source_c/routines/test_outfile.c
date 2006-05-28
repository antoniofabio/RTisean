/*Author: Rainer Hegger Last modified: Mar 20, 1999 */
#include <stdio.h>
#include <stdlib.h>
#include "tisean_cec.h"

void test_outfile(char *name)
{
  FILE *file;
  
  file=fopen(name,"a");
  if (file == NULL) {
    fprintf(stderr,"Couldn't open %s for writing. Exiting\n",name);
    exit(TEST_OUTFILE_NO_WRITE_ACCESS);
  }
  fclose(file);
}
