/*Author: Rainer Hegger Last modified: Jul 9, 1999 */
#include <math.h>

unsigned long find_multi_neighbors(double **s,long **box,long *list,double **x,
			     unsigned long l,unsigned int bs,unsigned int dim,
			     unsigned int emb,unsigned int del,double eps,
			     unsigned long *flist)
{
  unsigned long nf=0;
  int i,i1,i2,j,j1,k,k1,li;
  int ib=bs-1;
  long element;
  double dx;
  
  i=(int)(x[0][0]/eps)&ib;
  j=(int)(x[dim-1][0]/eps)&ib;
  
  for (i1=i-1;i1<=i+1;i1++) {
    i2=i1&ib;
    for (j1=j-1;j1<=j+1;j1++) {
      element=box[i2][j1&ib];
      while (element != -1) {
	for (k=0;k<emb;k++) {
	  k1= -k*(int)del;
	  for (li=0;li<dim;li++) {
	    dx=fabs(x[li][k1]-s[li][element+k1]);
	    if (dx > eps)
	      break;
	  }
	  if (dx > eps)
	    break;
	}
	if (dx <= eps)
	  flist[nf++]=element;
	element=list[element];
      }
    }
  }
  return nf;
}
