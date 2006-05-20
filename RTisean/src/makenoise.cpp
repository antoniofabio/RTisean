/*Author: Rainer Hegger Last modified: Sep 29, 2000 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "makenoise.h"

#define WID_STR "Adds noise to a time series or just creates random numbers"


void ns_makenoise::show_options(char *progname)
{
  longjmp(my_exit_point,4);
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [Options]\n\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of points to be used [Default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [Default: %lu]\n",exclude);
  fprintf(stderr,"\t-m # of columns to read [Default: %u]\n",dim);
  fprintf(stderr,"\t-c column(s) to read  [Default: 1]\n");
  fprintf(stderr,"\t-%% noiselevel in %% [Default:  %.1e%%]\n",
	  noiselevel*100.0);
  fprintf(stderr,"\t-r absolute noise level (or absolute variance in case\n"
	  "\t\tof gaussian noise) [Default: not set]\n");
  fprintf(stderr,"\t-g (use gaussian noise)     [Default: uniform]\n");
  fprintf(stderr,"\t-I seed for the rnd-generator (If seed=0, the time\n"
	  "\t\tcommand is used to set the seed) [Default: fixed]\n");
  fprintf(stderr,"\t-0 do not read input, just generate random numbers\n\t\t"
	  "(needs -l and -r) [Default: not set]\n");
  fprintf(stderr,"\t-o outfile [Without argument 'datafile'.noi;"
	  " Without -o stdout is used]\n");
  fprintf(stderr,"\t-V verbosity level [Default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"  -h show these options");
  fprintf(stderr,"\n");
  exit(0);
}

void ns_makenoise::scan_options(int n,char** in)
{
  char *out,lengthset=0;
  
  if ((out=check_option(in,n,'l','u')) != NULL) {
    sscanf(out,"%lu",&length);
    lengthset=1;
  }
  if ((out=check_option(in,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(in,n,'m','u')) != NULL) {
    sscanf(out,"%u",&dim);
    dimset=1;
  }
  if ((out=check_option(in,n,'c','s')) != NULL)
    column=out;
  if ((out=check_option(in,n,'%','f')) != NULL) {
    sscanf(out,"%lf",&noiselevel);
    noiselevel /= 100.0;
  }
  if ((out=check_option(in,n,'r','f')) != NULL) {
    sscanf(out,"%lf",&noiselevel);
    absolute=1;
  }
  if ((out=check_option(in,n,'g','n')) != NULL)
    cgaussian=1;
  if ((out=check_option(in,n,'I','u')) != NULL) {
    sscanf(out,"%lu",&iseed);
    if (iseed == 0)
      iseed=(unsigned long)time((time_t*)&iseed);
  }
  if ((out=check_option(in,n,'0','n')) != NULL) {
    if (absolute && lengthset)
      justcreate=1;
    else {
      longjmp(my_exit_point,4);
      fprintf(stderr,"\nThe -0 flag requires -l and -r\n\n");
      exit(MAKENOISE__FLAGS_REQUIRED);
    }
  }

  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stout=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

void ns_makenoise::equidistri(double sigmax,unsigned int which) 
{
  int i;
  double limit,equinorm;
  
  equinorm=(double)ULONG_MAX;
  if (!absolute)
    limit=2.0*sqrt(3.0)*sigmax*noiselevel;
  else
    limit=2.0*noiselevel;
  for (i=0;i<length;i++)
    array[which][i] += (limit*((double)rnd_1279()/equinorm-0.5));
} 

void ns_makenoise::gauss(double sigmax,unsigned int which)
{
  int i;
  double glevel;

  if (!absolute)
    glevel=noiselevel*sigmax;
  else
    glevel=noiselevel;
  for (i=0;i<length;i++)
    array[which][i] += gaussian(glevel);
}

int ns_makenoise::main(int argc,char** argv)
{

outfile=NULL;stout=1;justcreate=0;
infile=NULL;
absolute=0;dimset=0;
length=ULONG_MAX;exclude=0;iseed=3441341;
dim=1;
column=NULL;
verbosity=0xff;
noiselevel=0.05;


  char stdi=0;
  unsigned long i,j;
  double av=0.0,*sigmax;
  FILE *fout;

  if (scan_help(argc,argv))
    show_options(argv[0]);

  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  if (!justcreate) {
    infile=search_datafile(argc,argv,NULL,verbosity);
    if (infile == NULL)
      stdi=1;
  }
  else
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+5,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".noi");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      strcpy(outfile,"stdin.noi");
    }
  }
  if (!stout)
    test_outfile(outfile);

  if (!justcreate) {
    if (column == NULL)
      array=(double**)get_multi_series(infile,&length,exclude,&dim,"",dimset,
				       verbosity);
    else
      array=(double**)get_multi_series(infile,&length,exclude,&dim,column,
				       dimset,verbosity);
  }
  else {
    check_alloc(array=(double**)malloc(sizeof(double*)*dim));
    for (i=0;i<dim;i++) {
      check_alloc(array[i]=(double*)malloc(sizeof(double)*length));
      for (j=0;j<length;j++)
	array[i][j]=0.0;
    }
  }

  check_alloc(sigmax=(double*)malloc(sizeof(double)*dim));

  if (!absolute) {
    for (j=0;j<dim;j++)
      variance(array[j],length,&av,&sigmax[j]);
  }

  rnd_init(iseed);

  for (i=0;i<10000;i++) rnd_1279();

  for (j=0;j<dim;j++) {
    if (!cgaussian)
      equidistri(sigmax[j],j);
    else
      gauss(sigmax[j],j);
  }

  if (!stout) {
    fout=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
    for (i=0;i<length;i++) {
      for (j=0;j<dim-1;j++)
	fprintf(fout,"%e ",array[j][i]);
      fprintf(fout,"%e\n",array[dim-1][i]);
    }
    fclose(fout);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
    for (i=0;i<length;i++) {
      for (j=0;j<dim-1;j++)
	fprintf(stdout,"%e ",array[j][i]);
      fprintf(stdout,"%e\n",array[dim-1][i]);
    }
  }

  for (i=0;i<dim;i++)
    free(array[i]);
  free(array);
  free(sigmax);
  if (outfile != NULL)
    free(outfile);
  if (infile != NULL)
    free(infile);
  if (column != NULL)
    free(column);

  return 0;
}


