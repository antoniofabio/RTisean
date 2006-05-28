/* Author: Rainer Hegger Last modified: Jul 15, 1999 */
#include <stdlib.h>
#include <stdio.h>
#include "tisean_cec.h"

void check_alloc(void *pnt)
{
  if (pnt == NULL) {
    fprintf(stderr,"check_alloc: Couldn't allocate enough memory. Exiting\n");
    exit(CHECK_ALLOC_NOT_ENOUGH_MEMORY);
  }
}
