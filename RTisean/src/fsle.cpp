/*Author: Rainer Hegger, last modified: Mar 20, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>
#include <limits.h>
#include <string.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "fsle.h"

#define WID_STR "Estimates the finite size Lyapunov exponent; Vulpiani et al."


#define NMAX 256


struct fsle {
  double time,factor,eps;
  long count;
} *data;

void ns_fsle::show_options(char *progname)
{
  longjmp(my_exit_point,4);
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of datapoints [default is whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [default is 0]\n");
  fprintf(stderr,"\t-c column to read[default 1]\n");
  fprintf(stderr,"\t-m embedding dimension [default 2]\n");
  fprintf(stderr,"\t-d delay  [default 1]\n");
  fprintf(stderr,"\t-t time window to omit [default 0]\n");
  fprintf(stderr,"\t-r epsilon size to start with [default "
	  "(std. dev. of data)/1000]\n");
  fprintf(stderr,"\t-o name of output file [default 'datafile'.fsl ,"
	  "without -o: stdout]\n");
  fprintf(stderr,"\t-V verbosity level [default 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  fprintf(stderr,"\n");
  exit(0);
}

void ns_fsle::scan_options(int n,char **argv)
{
  char *out;

  if ((out=check_option(argv,n,'l','u')) != NULL)
    sscanf(out,"%lu",&length);
  if ((out=check_option(argv,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(argv,n,'c','u')) != NULL)
    sscanf(out,"%u",&column);
  if ((out=check_option(argv,n,'m','u')) != NULL)
    sscanf(out,"%u",&dim);
  if ((out=check_option(argv,n,'d','u')) != NULL)
    sscanf(out,"%u",&delay);
  if ((out=check_option(argv,n,'t','u')) != NULL)
    sscanf(out,"%u",&mindist);
  if ((out=check_option(argv,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(argv,n,'r','f')) != NULL) {
    epsset=1;
    sscanf(out,"%lf",&eps0);
  }
  if ((out=check_option(argv,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}
      
void ns_fsle::put_in_boxes(void)
{
  int i,j,x,y,del;

  for (i=0;i<NMAX;i++)
    for (j=0;j<NMAX;j++)
      box[i][j]= -1;

  del=delay*(dim-1);
  for (i=0;i<length-del;i++) {
    x=(int)(series[i]*epsinv)&nmax;
    y=(int)(series[i+del]*epsinv)&nmax;
    list[i]=box[x][y];
    box[x][y]=i;
  }
}

char ns_fsle::make_iterate(long act)
{
  char ok=0;
  int x,y,i,j,i1,k,del1=dim*delay,which;
  long element,minelement= -1;
  double dx=0.0,mindx=2.0,stime;

  x=(int)(series[act]*epsinv)&nmax;
  y=(int)(series[act+delay*(dim-1)]*epsinv)&nmax;
  for (i=x-1;i<=x+1;i++) {
    i1=i&nmax;
    for (j=y-1;j<=y+1;j++) {
      element=box[i1][j&nmax];
      while (element != -1) {
	if (labs(act-element) > mindist) {
	  for (k=0;k<del1;k+=delay) {
	    dx = fabs(series[act+k]-series[element+k]);
	    if (dx > eps)
	      break;
	  }
	  if (k==del1) {
	    if (dx < mindx) {
	      ok=1;
	      if (dx > 0.0) {
		mindx=dx;
		minelement=element;
	      }
	    }
	  }
	}
	element=list[element];
      }
    }
  }
  
  if ((minelement != -1) && (mindx < eps)) {
    act += del1-delay+1;
    minelement += del1-delay+1;
    which=(int)(log(mindx/eps0)/log(epsfactor));
    if (which < 0) {
      while ((dx=fabs(series[act]-series[minelement])) < data[0].eps) {
	act++;
	minelement++;
	if ((act >= length) || (minelement >= length))
	  return ok;
      }
      mindx=dx;
      which=(int)(log(mindx/eps0)/log(epsfactor));
    }
    for (i=which;i<howmany-1;i++) {
      stime=0;
      while ((dx=fabs(series[act]-series[minelement])) < data[i+1].eps) {
	act++;
	minelement++;
	if ((act >= length) || (minelement >= length))
	  return ok;
	stime++;
      }
      if (stime > 0) {
	data[i].time += stime;
	data[i].factor += log(dx/mindx);
	data[i].count++;
      }
      mindx=dx;
    }
  }
  return ok;
}

int ns_fsle::main(int argc,char **argv)
{
outfile=NULL;
infile=NULL;
epsset=0;stdo=1;
dim=2;delay=1;mindist=0;
column=1;
verbosity=0xff;
nmax=NMAX-1;
length=ULONG_MAX;exclude=0;
eps0=1.e-3;


  char stdi=0,*done,alldone;
  int i;
  long n;
  long maxlength;
  double min,max,se_av,se_var,se0_av,se0_var;
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
      strcpy(outfile,infile);
      strcat(outfile,".fsl");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      strcpy(outfile,"stdin.fsl");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  series=(double*)get_series(infile,&length,exclude,column,verbosity);
  variance(series,length,&se0_av,&se0_var);
  rescale_data(series,length,&min,&max);
  variance(series,length,&se_av,&se_var);
  
  if (epsset) {
    eps0 /= max;
    epsmax=se0_var;
  }
  else {
    eps0 *= se_var;
    epsmax=se_var;
  }
  if (eps0 >= epsmax) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"The minimal epsilon is too large. Exiting!\n");
    exit(FSLE__TOO_LARGE_MINEPS);
  }
  epsfactor=sqrt(2.0);

  howmany=(int)(log(epsmax/eps0)/log(epsfactor))+1;
  check_alloc(data=(struct fsle*)malloc(sizeof(struct fsle)*howmany));
  eps=eps0/epsfactor;
  for (i=0;i<howmany;i++) {
    data[i].time=data[i].factor=0.0;
    data[i].eps= (eps *= epsfactor);
    data[i].count=0;
  }
  
  check_alloc(list=(long*)malloc(length*sizeof(long)));
  check_alloc(done=(char*)malloc(length));

  for (i=0;i<length;i++)
    done[i]=0;
  
  maxlength=length-delay*(dim-1)-1-mindist;
  alldone=0;
  for (eps=eps0;(eps<=epsmax) && (!alldone);eps*=epsfactor) {
    epsinv=1.0/eps;
    put_in_boxes();
    alldone=1;
    for (n=0;n<=maxlength;n++) {
      if (!done[n])
	done[n]=make_iterate(n);
      alldone &= done[n];
    }
  } 
  if (!stdo) {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
    for (i=0;i<howmany;i++)
      if (data[i].factor > 0.0)
	fprintf(file,"%e %e %ld\n",data[i].eps*max,
		data[i].factor/data[i].time,data[i].count);
    fclose(file);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
    for (i=0;i<howmany;i++)
      if (data[i].factor > 0.0)
	fprintf(stdout,"%e %e %ld\n",data[i].eps*max,
		data[i].factor/data[i].time,data[i].count);
  }    

  if (infile != NULL)
    free(infile);
  if (outfile != NULL)
    free(outfile);
  free(series);
  free(data);
  free(list);
  free(done);

  return 0;
}

