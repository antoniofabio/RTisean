/*Author: Rainer Hegger. Last modified: Nov 22, 2000 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "tsa.h"
#include <math.h>

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "onestep.h"

#define WID_STR "Estimates the average forecast error for a local\n\t\
linear fit"


/*number of boxes for the neighbor search algorithm*/
#define NMAX 128


void ns_onestep::show_options(char *progname)
{
  longjmp(my_exit_point,4);
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
  fprintf(stderr,"\t-n iterations [default: length]\n");
  fprintf(stderr,"\t-k minimal number of neighbors for the fit "
	  "[default: 30]\n");
  fprintf(stderr,"\t-r neighborhoud size to start with "
	  "[default: (data interval)/1000]\n");
  fprintf(stderr,"\t-f factor to increase size [default: 1.2]\n");
  fprintf(stderr,"\t-s steps to forecast [default: 1]\n");
  fprintf(stderr,"\t-C width of causality window [default: steps]\n");
  fprintf(stderr,"\t-V verbosity level [default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  exit(0);
}

void ns_onestep::scan_options(int n,char **in)
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
  if ((out=check_option(in,n,'n','u')) != NULL)
    sscanf(out,"%lu",&CLENGTH);
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'k','u')) != NULL)
    sscanf(out,"%u",&MINN);
  if ((out=check_option(in,n,'r','f')) != NULL) {
    epsset=1;
    sscanf(out,"%lf",&EPS0);
  }
  if ((out=check_option(in,n,'f','f')) != NULL)
    sscanf(out,"%lf",&EPSF);
  if ((out=check_option(in,n,'s','u')) != NULL)
    sscanf(out,"%u",&STEP);
  if ((out=check_option(in,n,'C','u')) != NULL) {
    sscanf(out,"%u",&causal);
    causalset=1;
  }
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

double ns_onestep::make_fit(long act,unsigned long number)
{
  double **mat,*vec,hs,casted;
  int i,j,k,which;
  
  check_alloc(vec=(double*)malloc(sizeof(double)*(DIM+1)));
  check_alloc(mat=(double**)malloc(sizeof(double*)*(DIM+1)));
  for (i=0;i<=DIM;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*(DIM+1)));
    
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

  free(vec);
  for (i=0;i<=DIM;i++)
    free(mat[i]);
  free(mat);

  return (casted-series[act+STEP])*(casted-series[act+STEP]);
}

int ns_onestep::main(int argc,char **argv)
{

nmax=(NMAX-1);

epsset=0;causalset=0;
COLUMN=1;
verbosity=0xff;
DIM=2;DELAY=1;MINN=30;STEP=1;
EPS0=1.e-3;EPSF=1.2;
LENGTH=ULONG_MAX;exclude=0;CLENGTH=ULONG_MAX;
infile=NULL;


char alldone,*done;
  long i;
  unsigned long *hfound;
  unsigned long actfound;
  unsigned long clength;
  double rms,av,error=0.0;


  if (scan_help(argc,argv))
    show_options(argv[0]);
  
  scan_options(argc,argv);

  if (!causalset)
    causal=STEP;

#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  infile=search_datafile(argc,argv,&COLUMN,verbosity);
  
  series=(double*)get_series(infile,&LENGTH,exclude,COLUMN,verbosity);
  rescale_data(series,LENGTH,&min,&interval);
  variance(series,LENGTH,&av,&rms);
  
  check_alloc(list=(long*)malloc(sizeof(long)*LENGTH));
  check_alloc(found=(unsigned long*)malloc(sizeof(long)*LENGTH));
  check_alloc(hfound=(unsigned long*)malloc(sizeof(long)*LENGTH));
  check_alloc(done=(char*)malloc(sizeof(char)*LENGTH));
  check_alloc(box=(long**)malloc(sizeof(long*)*NMAX));
  for (i=0;i<NMAX;i++)
    check_alloc(box[i]=(long*)malloc(sizeof(long)*NMAX));
    
  for (i=0;i<LENGTH;i++)
    done[i]=0;

  alldone=0;
  if (epsset)
    EPS0 /= interval;

  epsilon=EPS0/EPSF;
  clength=(CLENGTH <= LENGTH) ? CLENGTH-STEP : LENGTH-STEP;

  while (!alldone) {
    alldone=1;
    epsilon*=EPSF;
    make_box(series,box,list,LENGTH-STEP,NMAX,(unsigned int)DIM,
	     (unsigned int)DELAY,epsilon);
    for (i=(DIM-1)*DELAY;i<clength;i++)
      if (!done[i]) {
	actfound=find_neighbors(series,box,list,series+i,LENGTH,NMAX,
				(unsigned int)DIM,(unsigned int)DELAY,
				epsilon,hfound);
	actfound=exclude_interval(actfound,i-causal+1,i+causal+(DIM-1)*DELAY-1,
				  hfound,found);
	if (actfound > MINN) {
	  error += make_fit(i,actfound);
	  done[i]=1;
	}
	alldone &= done[i];
      }
  }

  FILE* file=NULL;
  if (!stdo) {
    file=fopen(outfile,"w");
    fprintf(file,"Relative forecast error= %e\n",
	 sqrt(error/(clength-(DIM-1)*DELAY))/rms);
    fclose(file);
  }
  return 0;
}



