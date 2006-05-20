/*Author: Rainer Hegger, Last modified: Dec 12, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "ar-model.h"

#define WID_STR "Fits an multivariate AR model to the data and gives\
 the coefficients\n\tand the residues (or an iterated model)"


void ns_ar_model::show_options(char *progname)
{
  longjmp(my_exit_point,4);
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
	  " as a possible"
	  " datafile.\nIf no datafile is given stdin is read. Just - also"
	  " means stdin\n");
  fprintf(stderr,"\t-l length of file [default is whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [default is 0]\n");
  fprintf(stderr,"\t-m dimension [default is 1]\n");
  fprintf(stderr,"\t-c columns to read [default is 1,...,dimension]\n");
  fprintf(stderr,"\t-p #order of AR-Fit [default is 1]\n");
  fprintf(stderr,"\t-s length of iterated model [default no iteration]\n");
  fprintf(stderr,"\t-o output file name [default is 'datafile'.ar]\n");
  fprintf(stderr,"\t-V verbosity level [default is 1]\n\t\t"
	  "0='only panic messages'\n\t\t"
	  "1='+ input/output messages'\n\t\t"
	  "2='+ print residuals though iterating a model'\n");
  fprintf(stderr,"\t-h show these options\n\n");
  exit(0);
}

void ns_ar_model::scan_options(int argc,char **argv)
{
  char *out;

  if ((out=check_option(argv,argc,'p','u')) != NULL)
    sscanf(out,"%u",&poles);
  if ((out=check_option(argv,argc,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(argv,argc,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(argv,argc,'m','u')) != NULL) {
    sscanf(out,"%u",&dim);
    dimset=1;
  }
  if ((out=check_option(argv,argc,'c','u')) != NULL)
    column=out;
  if ((out=check_option(argv,argc,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(argv,argc,'s','u')) != NULL) {
    sscanf(out,"%u",&ilength);
    run_model=1;
  }
  if ((out=check_option(argv,argc,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

void ns_ar_model::set_averages_to_zero(void)
{
  double av,var;
  long i,j;
  
  for (i=0;i<dim;i++) {
    variance(series[i],length,&av,&var);
    for (j=0;j<length;j++)
      series[i][j] -= av;
  }
}

double** ns_ar_model::build_matrix(double **mat)
{
  long n,i1,j1,i2,j2,hi,hj;
  double norm;
  
  norm=1./((double)length-(double)poles);

  for (i1=0;i1<dim;i1++)
    for (i2=0;i2<poles;i2++) {
      hi=i1*poles+i2;
      for (j1=0;j1<dim;j1++)
	for (j2=0;j2<poles;j2++) {
	  hj=j1*poles+j2;
	  mat[hi][hj]=0.0;
	  for (n=poles-1;n<length-1;n++)
	    mat[hi][hj] += series[i1][n-i2]*series[j1][n-j2];
	  mat[hi][hj] *= norm;
	}
    }

  return invert_matrix(mat,(unsigned int)(dim*poles));
}

void ns_ar_model::build_vector(double *vec,long comp)
{
  long i1,i2,hi,n;
  double norm;

  norm=1./((double)length-(double)poles);

  for (i1=0;i1<poles*dim;i1++)
    vec[i1]=0.0;
  
  for (i1=0;i1<dim;i1++)
    for (i2=0;i2<poles;i2++) {
      hi=i1*poles+i2;
      for (n=poles-1;n<length-1;n++)
	vec[hi] += series[comp][n+1]*series[i1][n-i2];
      vec[hi] *= norm;
    }
}

double* ns_ar_model::multiply_matrix_vector(double **mat,double *vec)
{
  long i,j;
  double *ynew;

  check_alloc(ynew=(double*)malloc(sizeof(double)*poles*dim));

  for (i=0;i<poles*dim;i++) {
    ynew[i]=0.0;
    for (j=0;j<poles*dim;j++)
      ynew [i] += mat[i][j]*vec[j];
  }
  return ynew;
}

double* ns_ar_model::make_residuals(double **diff,double **coeff)
{
  long n,d,i,j;
  double *resi;
  
  check_alloc(resi=(double*)malloc(sizeof(double)*dim));
  for (i=0;i<dim;i++)
    resi[i]=0.0;

  for (n=poles-1;n<length-1;n++) {
    for (d=0;d<dim;d++) {
      diff[d][n+1]=0.0;
      for (i=0;i<dim;i++)
	for (j=0;j<poles;j++)
	  diff[d][n+1] += coeff[d][i*poles+j]*series[i][n-j];
      diff[d][n+1] -= series[d][n+1];
      resi[d] += sqr(diff[d][n+1]);
    }
  }
  for (i=0;i<dim;i++)
    resi[i]=sqrt(resi[i]/((double)length-(double)poles));

  return resi;
}

void ns_ar_model::iterate_model(double **coeff,double *sigma,FILE *file)
{
  long i,j,i1,i2,n,d;
  double **iterate,*swap;
  
  check_alloc(iterate=(double**)malloc(sizeof(double*)*(poles+1)));
  for (i=0;i<=poles;i++)
    check_alloc(iterate[i]=(double*)malloc(sizeof(double)*dim));
  rnd_init(0x44325);
  for (i=0;i<1000;i++)
    gaussian(1.0);
  for (i=0;i<dim;i++)
    for (j=0;j<poles;j++)
      iterate[j][i]=gaussian(sigma[i]);
  
  for (n=0;n<ilength;n++) {
    for (d=0;d<dim;d++) {
      iterate[poles][d]=gaussian(sigma[d]);
      for (i1=0;i1<dim;i1++)
	for (i2=0;i2<poles;i2++)
	  iterate[poles][d] += coeff[d][i1*poles+i2]*iterate[poles-1-i2][i1];
    }
    if (file != NULL) {
      for (d=0;d<dim;d++)
	fprintf(file,"%e ",iterate[poles][d]);
      fprintf(file,"\n");
    }
    else {
      for (d=0;d<dim;d++)
	printf("%e ",iterate[poles][d]);
      printf("\n");
    }

    swap=iterate[0];
    for (i=0;i<poles;i++)
      iterate[i]=iterate[i+1];
    iterate[poles]=swap;
  }

  for (i=0;i<=poles;i++)
    free(iterate[i]);
  free(iterate);
}

int ns_ar_model::main(int argc,char **argv)
{


	length=ULONG_MAX;exclude=0;
	dim=1;poles=1;ilength;
	verbosity=1;
	outfile=NULL;column=NULL;stdo=1;dimset=0;run_model=0;
	infile=NULL;
	

  char stdi=0;
  double *pm;
  long i,j;
  FILE *file;
  double **mat,**inverse,*vec,**coeff,**diff;
  
  if (scan_help(argc,argv))
    show_options(argv[0]);
  
  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  infile=search_datafile(argc,argv,NULL,verbosity);
  if (infile == NULL)
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+4,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".ar");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)9,(size_t)1));
      strcpy(outfile,"stdin.ar");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  if (column == NULL)
    series=(double**)get_multi_series(infile,&length,exclude,&dim,"",dimset,
				      verbosity);
  else
    series=(double**)get_multi_series(infile,&length,exclude,&dim,column,
				      dimset,verbosity);

  set_averages_to_zero();

  if (poles >= length) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"It makes no sense to have more poles than data! Exiting\n");
    exit(AR_MODEL_TOO_MANY_POLES);
  }
  
  
  check_alloc(vec=(double*)malloc(sizeof(double)*poles*dim));
  check_alloc(mat=(double**)malloc(sizeof(double*)*poles*dim));
  for (i=0;i<poles*dim;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*poles*dim));

  check_alloc(coeff=(double**)malloc(sizeof(double*)*dim));
  inverse=build_matrix(mat);
  for (i=0;i<dim;i++) {
    build_vector(vec,i);
    coeff[i]=multiply_matrix_vector(inverse,vec);
  }

  check_alloc(diff=(double**)malloc(sizeof(double*)*dim));
  for (i=0;i<dim;i++)
    check_alloc(diff[i]=(double*)malloc(sizeof(double)*length));

  pm=make_residuals(diff,coeff);
  
  if (stdo) {
    printf("#forecast errors: ");
    for (i=0;i<dim;i++)
      printf("%e ",pm[i]);
    printf("\n");
    for (i=0;i<dim*poles;i++) {
      printf("# ");
      for (j=0;j<dim;j++)
	printf("%e ",coeff[j][i]);
      printf("\n");
    }
    if (!run_model || (verbosity&VER_USR1)) {
      for (i=poles;i<length;i++) {
	if (run_model)
	  printf("#");
	for (j=0;j<dim;j++)
	  printf("%e ",diff[j][i]);
	printf("\n");
      }
    }
    if (run_model && (ilength > 0))
      iterate_model(coeff,pm,NULL);
  }
  else {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for output\n",outfile);
    fprintf(file,"#forecast errors: ");
    for (i=0;i<dim;i++)
      fprintf(file,"%e ",pm[i]);
    fprintf(file,"\n");
    for (i=0;i<dim*poles;i++) {
      fprintf(file,"# ");
      for (j=0;j<dim;j++)
	fprintf(file,"%e ",coeff[j][i]);
      fprintf(file,"\n");
    }
    if (!run_model || (verbosity&VER_USR1)) {
      for (i=poles;i<length;i++) {
	if (run_model)
	  fprintf(file,"#");
	for (j=0;j<dim;j++)
	  fprintf(file,"%e ",diff[j][i]);
	fprintf(file,"\n");
      }
    }
    if (run_model && (ilength > 0))
      iterate_model(coeff,pm,file);
    fclose(file);
  }

  if (outfile != NULL)
    free(outfile);
  if (infile != NULL)
    free(infile);
  free(vec);
  for (i=0;i<poles*dim;i++) {
    free(mat[i]);
    free(inverse[i]);
  }
  free(mat);
  free(inverse);
  for (i=0;i<dim;i++) {
    free(coeff[i]);
    free(diff[i]);
  }
  free(coeff);
  free(diff);
  free(pm);

  return 0;
}

