/*Author: Rainer Hegger Last modified: Mar 20, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include "tisean_cec.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

void test_outfile(char *name)
{
  FILE *file;
  
  file=fopen(name,"a");
  if (file == NULL) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Couldn't open %s for writing. Exiting\n",name);
    exit(TEST_OUTFILE_NO_WRITE_ACCESS);
  }
  fclose(file);
}

