/*Author: Rainer Hegger Last modified: May 26, 2000*/

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include "tisean_cec.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

void rescale_data(double *x,unsigned long l,double *min,double *interval)
{
  int i;
  
  *min=*interval=x[0];
  
  for (i=1;i<l;i++) {
    if (x[i] < *min) *min=x[i];
    if (x[i] > *interval) *interval=x[i];
  }
  *interval -= *min;

  if (*interval != 0.0) {
    for (i=0;i<l;i++)
      x[i]=(x[i]- *min)/ *interval;
  }
  else {
    longjmp(my_exit_point,4);
    fprintf(stderr,"rescale_data: data ranges from %e to %e. It makes\n"
	    "\t\tno sense to continue. Exiting!\n\n",*min,*min+(*interval));
    exit(RESCALE_DATA_ZERO_INTERVAL);
  }
}

