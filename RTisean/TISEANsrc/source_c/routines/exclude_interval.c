/*Author: Rainer Hegger Last modified: Apr 17, 1999 */
#include <stdio.h>
#include <stdlib.h>
#ifndef _MATH_H
#include <math.h>
#endif

unsigned long exclude_interval(unsigned long n,long ex0,long ex1,
			       unsigned long *hf,unsigned long *found)
{
  long i,help;
  long lf=0;
  
  for (i=0;i<n;i++) {
    help=hf[i];
    if ((help < ex0) || (help > ex1))
      found[lf++]=help;
  }
  return lf;
}
