/*Author: Rainer Hegger Last modified: Nov 30, 2000 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "routines/tsa.h"

#define WID_STR "Performs simple noise reduction."

#define BOX (unsigned int)512

unsigned long length=ULONG_MAX,exclude=0;
unsigned int column=1,dim=5,delay=1,iterations=1;
unsigned int verbosity=0x3;
double eps=1.0e-3,epsvar;

char *outfile=NULL,epsset=0,stdo=1,epsvarset=0;
char *infile=NULL;
double *series,*corr,interval,min;
long **box,*list,*nf;

void show_options(char *progname)
{
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [Options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of data to use [default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [default: 0]\n");
  fprintf(stderr,"\t-c column to read [default: 1]\n");
  fprintf(stderr,"\t-m embedding dimension [default: 5]\n");
  fprintf(stderr,"\t-d delay [default: 1]\n");
  fprintf(stderr,"\t-i iterations [default: 1]\n");
  fprintf(stderr,"\t-r neighborhoud size [default: (interval of data)/1000]\n");
  fprintf(stderr,"\t-v neighborhoud size (in units of the std. dev. of the "
	  "data \n\t\t(overwrites -r) [default: not set]\n");
  fprintf(stderr,"\t-o output file name [Default: 'datafile'.laz.n,"
	  "\n\t\twhere n is the number of the last iteration,"
	  "\n\t\twithout -o the last iteration is written to stdout.]\n");
  fprintf(stderr,"\t-V verbosity level [Default: 3]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n\t\t"
	  "2='+ write output of all iterations to files'\n\t\t"
	  "4='+ write the number of neighbors found for each point\n");
  fprintf(stderr,"\t-h show these options\n");
  exit(0);
}

void scan_options(int n,char **in)
{
  char *out;

  if ((out=check_option(in,n,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(in,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(in,n,'c','u')) != NULL)
    sscanf(out,"%u",&column);
  if ((out=check_option(in,n,'m','u')) != NULL)
    sscanf(out,"%u",&dim);
  if ((out=check_option(in,n,'d','u')) != NULL)
    sscanf(out,"%u",&delay);
  if ((out=check_option(in,n,'i','u')) != NULL)
    sscanf(out,"%u",&iterations);
  if ((out=check_option(in,n,'r','f')) != NULL) {
    epsset=1;
    sscanf(out,"%lf",&eps);
  }
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'v','f')) != NULL) {
    epsvarset=1;
    sscanf(out,"%lf",&epsvar);
  }
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

unsigned int correct(unsigned long n)
{
  int i,i1,i2,j,j1,k,k1;
  int ibox=BOX-1;
  double epsinv,dx,*hcor;
  long element,nfound=0;
  
  epsinv=1./eps;
  check_alloc(hcor=(double*)malloc(sizeof(double)*dim));
  for (i=0;i<dim;i++)
    hcor[i]=0.0;

  i=(int)(series[n-(dim-1)*delay]*epsinv)&ibox;
  j=(int)(series[n]*epsinv)&ibox;
  
  for (i1=i-1;i1<=i+1;i1++) {
    i2=i1&ibox;
    for (j1=j-1;j1<=j+1;j1++) {
      element=box[i2][j1&ibox];
      while (element != -1) {
	for (k=0;k<dim;k++) {
	  k1=k*delay;
	  dx=fabs(series[n-k1]-series[element-k1]);
	  if (dx > eps)
	    break;
	}
	if (k == dim) {
	  nfound++;
	  for (k=0;k<dim;k++)
	    hcor[k] += series[element-k*delay];
	}
	element=list[element];
      }
    }
  }
  for (k=0;k<dim;k++) {
    corr[n-k*delay] += hcor[k]/nfound;
    nf[n-k*delay]++;
  }
  free(hcor);

  return nfound;
}

int main(int argc,char **argv)
{
  char *ofname;
  char stdi=0;
  int iter;
  unsigned int *nmf;
  unsigned long n;
  double dav,dvar;
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
      check_alloc(outfile=(char*)calloc(strlen(infile)+5,(size_t)1));
      check_alloc(ofname=(char*)calloc(strlen(infile)+9,(size_t)1));
      sprintf(outfile,"%s.laz",infile);
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      check_alloc(ofname=(char*)calloc((size_t)14,(size_t)1));
      sprintf(outfile,"stdin.laz");
    }
  }
  else
    check_alloc(ofname=(char*)calloc(strlen(outfile)+10,(size_t)1));

  series=(double*)get_series(infile,&length,exclude,column,verbosity);
  rescale_data(series,length,&min,&interval);
  variance(series,length,&dav,&dvar);

  check_alloc(corr=(double*)malloc(sizeof(double)*length));
  check_alloc(nmf=(unsigned int*)malloc(sizeof(int)*length));
  check_alloc(list=(long*)malloc(sizeof(long)*length));
  check_alloc(box=(long**)malloc(sizeof(long*)*BOX));
  for (n=0;n<BOX;n++)
    check_alloc(box[n]=(long*)malloc(sizeof(long)*BOX));

  check_alloc(nf=(long*)malloc(sizeof(long)*length));

  if (epsset)
    eps/=interval;
  else
    eps=1.0/1000.;

  if (epsvarset)
    eps=epsvar*dvar;

  for (iter=1;iter<=iterations;iter++) {
    make_box(series,box,list,length,BOX,dim,delay,eps);
    for (n=0;n<length;n++) {
      corr[n]=0.0;
      nf[n]=0;
      nmf[n]=1;
    }
    for (n=(dim-1)*delay;n<length;n++)
      nmf[n]=correct(n);
    
    for (n=0;n<length;n++)
      if (nf[n])
	series[n]=corr[n]/nf[n];

    if ((verbosity&VER_USR1) && (iter < iterations)) {
      sprintf(ofname,"%s.%d",outfile,iter);
      test_outfile(ofname);
      file=fopen(ofname,"w");
      if (verbosity&VER_INPUT)
	fprintf(stderr,"Opened %s for writing\n",ofname);
      if (stdo && (iter == iterations)) {
	if (verbosity&VER_INPUT)
	  fprintf(stderr,"Writing to stdout\n");
      }
      for (n=0;n<length;n++) {
	if (stdo && (iter == iterations)) {
	  if (verbosity&VER_USR2)
	    fprintf(stdout,"%e %u\n",series[n]*interval+min,nmf[n]);
	  else
	    fprintf(stdout,"%e\n",series[n]*interval+min);
	}
	if (verbosity&VER_USR2)
	  fprintf(file,"%e %u\n",series[n]*interval+min,nmf[n]);
	else
	  fprintf(file,"%e\n",series[n]*interval+min);
      }
      fclose(file);
    }
    if (iter == iterations) {
      if (!stdo || (verbosity&VER_USR1)) {
	sprintf(ofname,"%s.%d",outfile,iter);
	test_outfile(ofname);
	file=fopen(ofname,"w");
	if (verbosity&VER_INPUT)
	  fprintf(stderr,"Opened %s for writing\n",ofname);
	if (stdo && (iter == iterations)) {
	  if (verbosity&VER_INPUT)
	    fprintf(stderr,"Writing to stdout\n");
	}
      }
      for (n=0;n<length;n++) {
	if (stdo) {
	  if (verbosity&VER_USR2)
	    fprintf(stdout,"%e %u\n",series[n]*interval+min,nmf[n]);
	  else
	    fprintf(stdout,"%e\n",series[n]*interval+min);
	}
	if (!stdo || (verbosity&VER_USR1)) {
	  if (verbosity&VER_USR2)
	    fprintf(file,"%e %u\n",series[n]*interval+min,nmf[n]);
	  else
	    fprintf(file,"%e\n",series[n]*interval+min);
	}
      }
      if (!stdo || (verbosity&VER_USR1))
	fclose(file);
    }
  }
  return 0;
}
