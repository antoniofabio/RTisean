/* Author: Rainer Hegger Last modified: Jul 15, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */

#include <stdlib.h>
#include <stdio.h>
#include "tisean_cec.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

void check_alloc(void *pnt)
{
  if (pnt == NULL) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"check_alloc: Couldn't allocate enough memory. Exiting\n");
    exit(CHECK_ALLOC_NOT_ENOUGH_MEMORY);
  }
}

