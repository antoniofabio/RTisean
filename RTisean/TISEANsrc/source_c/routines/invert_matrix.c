/* Author: Rainer Hegger Last modified: Apr 3, 1999 */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

double **invert_matrix(double **mat,unsigned int size)
{
  int i,j,k;
  double **hmat,**imat,*vec;
  extern void solvele(double**,double*,unsigned int);

  check_alloc(hmat=(double**)malloc(sizeof(double*)*size));
  for (i=0;i<size;i++) {
    check_alloc(hmat[i]=(double*)malloc(sizeof(double)*size));
  }

  check_alloc(imat=(double**)malloc(sizeof(double*)*size));
  for (i=0;i<size;i++) {
    check_alloc(imat[i]=(double*)malloc(sizeof(double)*size));
  }

  check_alloc(vec=(double*)malloc(sizeof(double)*size));
  
  for (i=0;i<size;i++) {
    for (j=0;j<size;j++) {
      vec[j]=(i==j)?1.0:0.0;
      for (k=0;k<size;k++)
	hmat[j][k]=mat[j][k];
    }
    solvele(hmat,vec,size);
    for (j=0;j<size;j++)
      imat[j][i]=vec[j];
  }
  
  free(vec);
  for (i=0;i<size;i++)
    free(hmat[i]);
  free(hmat);

  return imat;
}
