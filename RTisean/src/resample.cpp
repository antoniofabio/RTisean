/*Author: Rainer Hegger, Last modified: Sep 4, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "resample.h"

#define WID_STR "Resample the data"



void ns_resample::show_options(char *progname)
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
  fprintf(stderr,"\t-c column to read [default is 1]\n");
  fprintf(stderr,"\t-s ynew sampling time (in units of the old one)"
	  " [default is %f]\n",sampletime);
  fprintf(stderr,"\t-p order of the interpolation [default is %d]\n",order);
  fprintf(stderr,"\t-o output file name [default is 'datafile'.rs]\n");
  fprintf(stderr,"\t-V verbosity level [default is 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n\n");
  exit(0);
}

void ns_resample::scan_options(int argc,char **argv)
{
  char *out;

  if ((out=check_option(argv,argc,'s','f')) != NULL)
    sscanf(out,"%lf",&sampletime);
  if ((out=check_option(argv,argc,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(argv,argc,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(argv,argc,'c','u')) != NULL)
    sscanf(out,"%u",&column);
  if ((out=check_option(argv,argc,'p','u')) != NULL)
    sscanf(out,"%u",&order);
  if ((out=check_option(argv,argc,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(argv,argc,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

int ns_resample::main(int argc,char **argv)
{
length=ULONG_MAX;exclude=0;
column=1;order=4;
verbosity=0xff;
outfile=NULL;stdo=1;
infile=NULL;
sampletime=0.5;


  char stdi=0;
  long i,j,itime,itime_old;
  int horder,horder2;
  double **mat,*vec,**imat,*coef;
  double time,htime,ynew;
  FILE *file=NULL;

  if (scan_help(argc,argv))
    show_options(argv[0]);
  
  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  infile=search_datafile(argc,argv,&column,verbosity);
  if (infile == NULL)
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+4,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".rs");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)9,(size_t)1));
      strcpy(outfile,"stdin.rs");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  series=(double*)get_series(infile,&length,exclude,column,verbosity);
  
  horder=order+1;
  horder2=(horder+1)/2-horder;

  check_alloc(mat=(double**)malloc(sizeof(double*)*horder));
  for (i=0;i<horder;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*horder));
  check_alloc(vec=(double*)malloc(sizeof(double)*horder));
  check_alloc(coef=(double*)malloc(sizeof(double)*horder));
  
  for (i=0;i<horder;i++)
    for (j=0;j<horder;j++)
      mat[i][j]=pow((double)(horder2+i),(double)j);

  imat=invert_matrix(mat,(unsigned int)horder);

  if (!stdo) {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
  }

  time=(horder+1)/2.;
  itime_old= -1;
  while (time < (double)(length-horder/2)) {
    itime=(int)time+horder2;
    if (itime != itime_old) {
      for (i=0;i<horder;i++)
	vec[i]=series[i+itime];
      for (i=0;i<horder;i++) {
	coef[i]=0.0;
	for (j=0;j<horder;j++)
	  coef[i] += imat[i][j]*vec[j];
      }
    }
    itime_old=itime;
    htime=time-itime+horder2;
    ynew=coef[0];
    for (i=1;i<horder;i++)
      ynew += coef[i]*pow(htime,(double)i);
    if (stdo)
      fprintf(stdout,"%e\n",ynew);
    else
      fprintf(file,"%e\n",ynew);
    time += sampletime;
  }
  if (!stdo)
    fclose(file);

  return 0;
}


