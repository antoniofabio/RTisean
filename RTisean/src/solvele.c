/* Author: Rainer Hegger Last modified: Aug 14th, 1998 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "tisean_cec.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

void solvele(double **mat,double *vec,unsigned int n)
{
  double vswap,*mswap,*hvec,max,h,pivot,q;
  int i,j,k,maxi;

  for (i=0;i<n-1;i++) {
    max=fabs(mat[i][i]);
    maxi=i;
    for (j=i+1;j<n;j++)
      if ((h=fabs(mat[j][i])) > max) {
	max=h;
	maxi=j;
      }
    if (maxi != i) {
      mswap=mat[i];
      mat[i]=mat[maxi];
      mat[maxi]=mswap;
      vswap=vec[i];
      vec[i]=vec[maxi];
      vec[maxi]=vswap;
    }
    
    hvec=mat[i];
    pivot=hvec[i];
    if (fabs(pivot) == 0.0) {
      longjmp(my_exit_point,4);
      fprintf(stderr,"Singular matrix! Exiting!\n");
      exit(SOLVELE_SINGULAR_MATRIX);
    }
    for (j=i+1;j<n;j++) {
      q= -mat[j][i]/pivot;
      mat[j][i]=0.0;
      for (k=i+1;k<n;k++)
	mat[j][k] += q*hvec[k];
      vec[j] += q*vec[i];
    }
  }
  vec[n-1] /= mat[n-1][n-1];
  for (i=n-2;i>=0;i--) {
    hvec=mat[i];
    for (j=n-1;j>i;j--)
      vec[i] -= hvec[j]*vec[j];
    vec[i] /= hvec[i];
  }
}

