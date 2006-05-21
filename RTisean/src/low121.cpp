/*Author: Rainer Hegger Last modified: Sep 4, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "low121.h"

#define WID_STR "Simple lowpass filter in the time domain"



void ns_low121::show_options(char *progname)
{
  longjmp(my_exit_point,4);
  what_i_do(progname,WID_STR);
  fprintf(stderr,"Usage: %s [options]\n",progname);
  fprintf(stderr,"Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of points to use [Default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [Default: 0]\n");
  fprintf(stderr,"\t-c column to read [Default: 1]\n");
  fprintf(stderr,"\t-i # of iterations [Default: 1]\n");
  fprintf(stderr,"\t-V verbosity level [Default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n\t\t"
          "2='+ print each iteration to a separate file\n");
  fprintf(stderr,"\t-o output file name(s) [Default: 'datafile'.low.n,\n\t\t"
	  "where n is the number of the iteration.\n\t\t"
	  "without -o the last iteration is written to stdout.]\n");
  fprintf(stderr,"\t-h show these options\n");
  exit(0);
}

void ns_low121::scan_options(int n,char **in)
{
  char *out;

  if ((out=check_option(in,n,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(in,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(in,n,'c','u')) != NULL)
    sscanf(out,"%u",&column);
  if ((out=check_option(in,n,'i','u')) != NULL)
    sscanf(out,"%u",&iterations);
  if ((out=check_option(in,n,'V','d')) != NULL)
    sscanf(out,"%d",&verbosity);
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

int ns_low121::main(int argc,char **argv)
{

length=ULONG_MAX;exclude=0;
column=1;iterations=1;
verbosity=0x1;
outfile=NULL;stdo=1;
infile=NULL;




  char stdi=0;
  char *ofname;
  unsigned long i;
  unsigned int iter;
  FILE *file;

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
      check_alloc(ofname=(char*)calloc(strlen(infile)+9,(size_t)1));
      sprintf(outfile,"%s.low",infile);
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      check_alloc(ofname=(char*)calloc((size_t)14,(size_t)1));
      sprintf(outfile,"stdin.low");
    }
  }
  else
		check_alloc(ofname=(char*)calloc(strlen(outfile)+10,(size_t)1));
  
	series=(double*)get_series(infile,&length,exclude,column,verbosity);
	check_alloc(ynew=(double*)malloc(sizeof(double)*length));
  
	if (verbosity&VER_USR1) {
		for (iter=1;iter<=iterations;iter++) {
			for (i=1;i<length-1;i++)
				ynew[i]=(series[i-1]+2.0*series[i]+series[i+1])/4.;
			sprintf(ofname,"%s.%d",outfile,iter);
			test_outfile(ofname);
			file=fopen(ofname,"w");
			if (verbosity&VER_INPUT)
				fprintf(stderr,"Opened %s for writing\n",ofname);
			if (stdo && (iter == iterations))
				fprintf(stdout,"%e\n",series[0]);
			fprintf(file,"%e\n",series[0]);
			if (stdo && (iter == iterations)) {
				if (verbosity&VER_INPUT)
					fprintf(stderr,"Writing to stdout\n");
			}
			for (i=1;i<length-1;i++) {
				if (stdo && (iter == iterations))
					fprintf(stdout,"%e\n",series[i]=ynew[i]);
				fprintf(file,"%e\n",series[i]=ynew[i]);
			}
			if (stdo && (iter == iterations))
				fprintf(stdout,"%e\n",series[length-1]);
			fprintf(file,"%e\n",series[length-1]);
			fclose(file);
		}
	} else {
		for (iter=1;iter<=iterations;iter++) {
			for (i=1;i<length-1;i++) {
				ynew[i]=(series[i-1]+2.0*series[i]+series[i+1])/4.;
      }
      for (i=1;i<length-1;i++)
				series[i]=ynew[i];
		}
    if (!stdo) {
//			sprintf(ofname,"%s.%d",outfile,iterations);
			sprintf(ofname,"%s.%d",outfile,1);
			file=fopen(ofname,"w");
			if (verbosity&VER_INPUT)
				fprintf(stderr,"Opened %s for writing\n",ofname);
			for (i=0;i<length;i++)
				fprintf(file,"%e\n",series[i]);
			fclose(file);
		}
		else {
			if (verbosity&VER_INPUT)
				fprintf(stderr,"Writing to stdout\n");
			for (i=0;i<length;i++)
				fprintf(stdout,"%e\n",series[i]);
    }
  }
  return 0;
}
