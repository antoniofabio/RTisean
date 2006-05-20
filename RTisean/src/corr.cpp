/*Author: Rainer Hegger. Last modified: Sep 3, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "corr.h"

#define WID_STR "Estimates the autocorrelations of a data set"


void ns_corr::show_options(char *progname) 
{
  longjmp(my_exit_point,4);
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [Options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l length [default is whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [default 0]\n");
  fprintf(stderr,"\t-c column to read [default is 1]\n");
  fprintf(stderr,"\t-D corrlength  [default is 100]\n");
  fprintf(stderr,"\t-n don\'t normalize to the variance"
	  " of the data [not set]\n");
  fprintf(stderr,"\t-o output_file  [default is 'datafile'.cor; no -o"
  " means stdout]\n");
  fprintf(stderr,"\t-V verbosity level [default is 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  fprintf(stderr,"\n");
  exit(0);
}

void ns_corr::scan_options(int argc,char **argv)
{
  char *out;

  if ((out=check_option(argv,argc,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(argv,argc,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(argv,argc,'c','u')) != NULL)
    sscanf(out,"%u",&column);
  if ((out=check_option(argv,argc,'D','u')) != NULL)
    sscanf(out,"%lu",&tau);
  if ((out=check_option(argv,argc,'n','n')) != NULL)
    normalize=0;
  if ((out=check_option(argv,argc,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(argv,argc,'o','o')) != NULL) {
    stout=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

double ns_corr::corr(long i)
{
  long j;
  double c=0.0;
  
  for (j=0;j<(length-i);j++)
    c += array[j]*array[j+i];

  return c/(length-i);
}

int ns_corr::main(int argc,char** argv)
{

outfile=NULL;stout=1;normalize=1;
column=1;
verbosity=0xff;
tau=100;length=ULONG_MAX;exclude=0;
infile=NULL;


  char stdi=0;
  long i;
  FILE *fout=NULL;
  
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
      check_alloc(outfile=(char*)calloc(strlen(infile)+5,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".cor");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      strcpy(outfile,"stdin.cor");
    }
  }
  if (!stout)
    test_outfile(outfile);

  array=(double*)get_series(infile,&length,exclude,column,verbosity);

  if (tau >= length)
    tau=length-1;

  variance(array,length,&av,&var);

  if (normalize) {
    for (i=0;i<length;i++)
      array[i] -= av;
  }

  if (!stout) {
    fout=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
    fprintf(fout,"# average=%e\n",av);
    fprintf(fout,"# standard deviation=%e\n",var);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
    fprintf(stdout,"# average=%e\n",av);
    fprintf(stdout,"# standard deviation=%e\n",var);
  }
  if (normalize)
    var *= var;
  else
    var=1.0;

  for (i=0;i<=tau;i++)
    if (!stout) {
      fprintf(fout,"%ld %e\n",i,corr(i)/var);
      fflush(fout);
    }
    else {
      fprintf(stdout,"%ld %e\n",i,corr(i)/var);
      fflush(stdout);
    }
  if (!stout)
    fclose(fout);

  if (outfile != NULL)
    free(outfile);
  if (infile != NULL)
    free(infile);
  free(array);

  return 0;
}

