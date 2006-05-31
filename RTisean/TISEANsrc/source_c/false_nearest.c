/*Author: Rainer Hegger. Last modified: Sep 3, 1999 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "routines/tsa.h"

#define WID_STR "Determines the fraction of false nearest neighbors."

char *outfile=NULL;
char *infile=NULL;
char stdo=1;
unsigned long length=ULONG_MAX,exclude=0,theiler=0;
unsigned int column=1,delay=1,maxdim=5,mindim=1;
unsigned int verbosity=0xff;
double rt=10.0;
double eps0=1.0e-5;
double *series;
double aveps,vareps;
double varianz;

#define BOX 1024
int ibox=BOX-1;
long **box,*list;
unsigned long toolarge;

void show_options(char *progname)
{
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of data [default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to ignore [default: 0]\n");
  fprintf(stderr,"\t-c column to read [default: 1]\n");
  fprintf(stderr,"\t-m minimal embedding dimension [default: 1]\n");
  fprintf(stderr,"\t-M maximal embedding dimension [default: 5]\n");
  fprintf(stderr,"\t-d delay [default: 1]\n");
  fprintf(stderr,"\t-f escape factor [default: 10.0]\n");
  fprintf(stderr,"\t-t theiler window [default: 0]\n");
  fprintf(stderr,"\t-o output file [default: 'datafile'.fnn; without -o"
	  " stdout]\n");
  fprintf(stderr,"\t-V verbosity level [default: 3]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n\t\t"
          "2='+ information about the current state\n");
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
    sscanf(out,"%u",&mindim);
  if ((out=check_option(in,n,'M','u')) != NULL)
    sscanf(out,"%u",&maxdim);
  if ((out=check_option(in,n,'d','u')) != NULL)
    sscanf(out,"%u",&delay);
  if ((out=check_option(in,n,'f','f')) != NULL)
    sscanf(out,"%lf",&rt);
  if ((out=check_option(in,n,'t','u')) != NULL)
    sscanf(out,"%lu",&theiler);
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

char find_nearest(long n,unsigned int dim,double eps)
{
  int x,y,x1,x2,y1,i,i1;
  long element,which= -1;
  double dx,maxdx,mindx=1.1,factor;

  x=(int)(series[n-(dim-1)*delay]/eps)&ibox;
  y=(int)(series[n]/eps)&ibox;
  
  for (x1=x-1;x1<=x+1;x1++) {
    x2=x1&ibox;
    for (y1=y-1;y1<=y+1;y1++) {
      element=box[x2][y1&ibox];
      while (element != -1) {
	if (labs(element-n) > theiler) {
	  maxdx=fabs(series[n]-series[element]);
	  for (i=1;i<dim;i++) {
	    i1=i*delay;
	    dx=fabs(series[n-i1]-series[element-i1]);
	    if (dx > maxdx)
	      maxdx=dx;
	  }
	  if ((maxdx < mindx) && (maxdx > 0.0)) {
	    which=element;
	    mindx=maxdx;
	  }
	}
	element=list[element];
      }
    }
  }

  if ((which != -1) && (mindx <= eps) && (mindx <= varianz/rt)) {
    aveps += mindx;
    vareps += mindx*mindx;
    factor=fabs(series[n+1]-series[which+1])/mindx;
    if (factor > rt)
      toolarge++;
    return 1;
  }
  return 0;
}

int main(int argc,char **argv)
{
  char stdi=0;
  FILE *file=NULL;
  double min,inter,epsilon,av;
  char *nearest,alldone;
  long i;
  unsigned int dim;
  unsigned long donesofar;

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
      strcat(outfile,".fnn");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      strcpy(outfile,"stdin.fnn");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  series=(double*)get_series(infile,&length,exclude,column,verbosity);
  rescale_data(series,length,&min,&inter);
  variance(series,length,&av,&varianz);

  check_alloc(list=(long*)malloc(sizeof(long)*length));
  check_alloc(nearest=(char*)malloc(length));
  check_alloc(box=(long**)malloc(sizeof(long*)*BOX));
  for (i=0;i<BOX;i++)
    check_alloc(box[i]=(long*)malloc(sizeof(long)*BOX));

  if (!stdo) {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
  }

  for (dim=mindim;dim<=maxdim;dim++) {
    epsilon=eps0;
    toolarge=0;
    alldone=0;
    donesofar=0;
    aveps=0.0;
    vareps=0.0;
    for (i=0;i<length;i++)
      nearest[i]=0;
    if (verbosity&VER_USR1)
      fprintf(stderr,"Start for dimension=%u\n",dim);
    while (!alldone && (epsilon < 2.*varianz/rt)) {
      alldone=1;
      make_box(series,box,list,length-1,BOX,dim,delay,epsilon);
      for (i=(dim-1)*delay;i<length-1;i++)
	if (!nearest[i]) {
	  nearest[i]=find_nearest(i,dim,epsilon);
	  alldone &= nearest[i];
	  donesofar += (unsigned long)nearest[i];
	}
      if (verbosity&VER_USR1)
	fprintf(stderr,"Found %lu up to epsilon=%e\n",donesofar,epsilon*inter);
      epsilon*=sqrt(2.0);
      if (!donesofar)
	eps0=epsilon;
    }
    if (donesofar == 0) {
      fprintf(stderr,"Not enough points found!\n");
      exit(FALSE_NEAREST_NOT_ENOUGH_POINTS);
    }
    aveps *= (1./(double)donesofar);
    vareps *= (1./(double)donesofar);
    if (stdo) {
      fprintf(stdout,"%u %e %e %e\n",dim,(double)toolarge/(double)donesofar,
	      aveps,vareps);
      fflush(stdout);
    }
    else {
      fprintf(file,"%u %e %e %e\n",dim,(double)toolarge/(double)donesofar,
	      aveps,vareps);
      fflush(file);
    }
  }
  if (!stdo)
    fclose(file);

  if (infile != NULL)
    free(infile);
  if (outfile != NULL)
    free(outfile);
  free(series);
  free(list);
  free(nearest);
  for (i=0;i<BOX;i++)
    free(box[i]);
  free(box);

  return 0;
}
