/*Author: Rainer Hegger. Last modified: Nov 22, 2000 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "routines/tsa.h"
#include <math.h>

#define WID_STR "Estimates the average forecast error for a local\n\t\
linear fit as a function of the neighborhood size."


/*number of boxes for the neighbor search algorithm*/
#define NMAX 128

unsigned int nmax=(NMAX-1);
long **box,*list;
unsigned long *found;
double *series;

char eps0set=0,eps1set=0,causalset=0;
char *outfile=NULL,stdo=1;
unsigned int COLUMN=1;
unsigned int DIM=2,DELAY=1;
unsigned int verbosity=0xff;
int STEP=1;
double EPS0=1.e-3,EPS1=1.0,EPSF=1.2;
unsigned long LENGTH=ULONG_MAX,exclude=0,CLENGTH=ULONG_MAX,causal;
char *infile=NULL;
double **mat,*vec;

void show_options(char *progname)
{
  what_i_do(progname,WID_STR);
  fprintf(stderr," Usage: %s [options]\n",progname);
  fprintf(stderr," Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of data to use [default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [default: 0]\n");
  fprintf(stderr,"\t-c column to read [default: 1]\n");
  fprintf(stderr,"\t-m embedding dimension [default: 2]\n");
  fprintf(stderr,"\t-d delay [default: 1]\n");
  fprintf(stderr,"\t-i iterations [default: length]\n");
  fprintf(stderr,"\t-r neighborhood size to start with [default:"
	  " (interval of data)/1000)]\n");
  fprintf(stderr,"\t-R neighborhood size to end with [default:"
	  " interval of data]\n");
  fprintf(stderr,"\t-f factor to increase size [default: 1.2]\n");
  fprintf(stderr,"\t-s steps to forecast [default: 1]\n");
  fprintf(stderr,"\t-C width of causality window [default: steps]\n");
  fprintf(stderr,"\t-o output file name [default: 'datafile.ll']\n");
  fprintf(stderr,"\t-V verbosity level [default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  exit(0);
}

void scan_options(int n,char **in)
{
  char *out;

  if ((out=check_option(in,n,'l','u')) != NULL)
    sscanf(out,"%lu",&LENGTH);
  if ((out=check_option(in,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(in,n,'c','u')) != NULL)
    sscanf(out,"%u",&COLUMN);
  if ((out=check_option(in,n,'m','u')) != NULL)
    sscanf(out,"%u",&DIM);
  if ((out=check_option(in,n,'d','u')) != NULL)
    sscanf(out,"%u",&DELAY);
  if ((out=check_option(in,n,'i','u')) != NULL)
    sscanf(out,"%lu",&CLENGTH);
  if ((out=check_option(in,n,'r','f')) != NULL) {
    eps0set=1;
    sscanf(out,"%lf",&EPS0);
  }
  if ((out=check_option(in,n,'R','f')) != NULL) {
    eps1set=1;
    sscanf(out,"%lf",&EPS1);
  }
  if ((out=check_option(in,n,'f','f')) != NULL)
    sscanf(out,"%lf",&EPSF);
  if ((out=check_option(in,n,'s','u')) != NULL)
    sscanf(out,"%u",&STEP);
  if ((out=check_option(in,n,'C','u')) != NULL) {
    sscanf(out,"%u",&causal);
    causalset=1;
  }
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

double make_fit(long act,unsigned long number)
{
  double hs,casted;
  int i,j,k,which;
  
  for (i=0;i<=DIM;i++) {
    vec[i]=0.0;
    for (j=0;j<=DIM;j++)
      mat[i][j]=0.0;
  }
  
  for (k=0;k<number;k++) {
    which=found[k];
    vec[0] += series[which+STEP];
    for (i=1;i<=DIM;i++)
      mat[0][i] += series[which-(i-1)*DELAY];
  }
  mat[0][0]=(double)number;
  
  for (k=0;k<number;k++) {
    which=found[k];
    for (i=1;i<=DIM;i++) {
      hs=series[which-(i-1)*DELAY];
      vec[i] += series[which+STEP]*hs;
      for (j=i;j<=DIM;j++)
	mat[i][j] += series[which-(j-1)*DELAY]*hs;
    }
  }
  for (i=0;i<=DIM;i++) {
    vec[i] /= number;
    for (j=i;j<=DIM;j++) {
      mat[i][j] /= number;
      mat[j][i]=mat[i][j];
    }
  }

  solvele(mat,vec,(unsigned int)(DIM+1));

  casted=vec[0];
  for (i=1;i<=DIM;i++)
    casted += vec[i]*series[act-(i-1)*DELAY];
  
  return (casted-series[act+STEP])*(casted-series[act+STEP]);
}

int main(int argc,char **argv)
{
  char stdi=0;
  unsigned long actfound;
  unsigned long *hfound;
  long pfound,i;
  unsigned long clength;
  double interval,min;
  double epsilon;
  double rms,av,error=0.0,avfound,hrms,hav;
  FILE *file=NULL;

  if (scan_help(argc,argv))
    show_options(argv[0]);
  
  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  if (!causalset)
    causal=STEP;

  infile=search_datafile(argc,argv,&COLUMN,verbosity);
  if (infile == NULL)
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+4,(size_t)1));
      sprintf(outfile,"%s.ll",infile);
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)9,(size_t)1));
      sprintf(outfile,"stdin.ll");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  series=(double*)get_series(infile,&LENGTH,exclude,COLUMN,verbosity);
  rescale_data(series,LENGTH,&min,&interval);
  variance(series,LENGTH,&av,&rms);
  
  check_alloc(list=(long*)malloc(sizeof(long)*LENGTH));
  check_alloc(found=(unsigned long*)malloc(sizeof(long)*LENGTH));
  check_alloc(hfound=(unsigned long*)malloc(sizeof(long)*LENGTH));
  check_alloc(box=(long**)malloc(sizeof(long*)*NMAX));
  for (i=0;i<NMAX;i++)
    check_alloc(box[i]=(long*)malloc(sizeof(long)*NMAX));
  check_alloc(vec=(double*)malloc(sizeof(double)*(DIM+1)));
  check_alloc(mat=(double**)malloc(sizeof(double*)*(DIM+1)));
  for (i=0;i<=DIM;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*(DIM+1)));
  
  if (eps0set)
    EPS0 /= interval;
  if (eps1set)
    EPS1 /= interval;

  clength=(CLENGTH <= LENGTH) ? CLENGTH-STEP : LENGTH-STEP;

  if (!stdo) {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
    fprintf(file,"#1. size 2. relative forecast error 3. fraction of points\n"
	    "#4. av neighbors found 5. absolute variance of the points\n");
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
  }

  for (epsilon=EPS0;epsilon<EPS1*EPSF;epsilon*=EPSF) {
    pfound=0;
    error=0.0;
    avfound=0.0;
    hrms=hav=0.0;
    make_box(series,box,list,LENGTH-STEP,NMAX,DIM,DELAY,epsilon);
    for (i=(DIM-1)*DELAY;i<clength;i++) {
      actfound=find_neighbors(series,box,list,series+i,LENGTH,NMAX,DIM,DELAY,
			      epsilon,hfound);
      actfound=exclude_interval(actfound,i-causal+1,i+causal+(DIM-1)*DELAY-1,
				hfound,found);
      if (actfound > 2*(DIM+1)) {
	error += make_fit(i,actfound);
	pfound++;
	avfound += (double)(actfound-1);
	hrms += series[i+STEP]*series[i+STEP];
	hav += series[i+STEP];
      }
    }
    if (pfound > 1) {
      hav /= pfound;
      hrms=sqrt(fabs(hrms/(pfound-1)-hav*hav*pfound/(pfound-1)));
      error=sqrt(error/pfound)/hrms;
    }
    if (stdo) {
      if (pfound > 1) {
	fprintf(stdout,"%e %e %e %e %e\n",epsilon*interval,error,
		(double)pfound/(clength-(DIM-1)*DELAY),avfound/pfound,
		hrms*interval);
	fflush(stdout);
      }
    }
    else {
      if (pfound > 1) {
	fprintf(file,"%e %e %e %e %e\n",epsilon*interval,error,
		(double)pfound/(clength-(DIM-1)*DELAY),avfound/pfound,
		hrms*interval);
	fflush(file);
      }
    }
  }
  if (!stdo)
    fclose(file);
  
  return 0;
}
