/*Author: Rainer Hegger Last modified: Sep 3, 1999 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "routines/tsa.h"

#define WID_STR "Noise reduction using the GHKSS algorithm"


#define BOX (unsigned int)512

unsigned long length=ULONG_MAX,exclude=0;
unsigned int dim=5,qdim=3,delay=1,column=1,minn=30,iterations=1;
unsigned int verbosity=0xff;
double mineps,epsfac;
char eps_set=0,euclidean=0,resize_eps;
char *outfile=NULL,stdo=1;
char *infile=NULL;

double d_max,d_min;
double *series,*delta,**corr;
double *metric,trace;
long **box,*list;
unsigned long *flist;

/*these are global to save time*/
int *sorted;
double *av,**mat,*eig,*off;

void show_options(char *progname)
{
  what_i_do(progname,WID_STR);
  fprintf(stderr,"Usage: %s [options]\n",progname);
  fprintf(stderr,"Options:\n");
  fprintf(stderr,"Everything not being a valid option will be interpreted"
          " as a possible"
          " datafile.\nIf no datafile is given stdin is read. Just - also"
          " means stdin\n");
  fprintf(stderr,"\t-l # of data to use [Default: whole file]\n");
  fprintf(stderr,"\t-x # of lines to be ignored [Default: 0]\n");
  fprintf(stderr,"\t-c column to read [Default: 1]\n");
  fprintf(stderr,"\t-m embedding dimension [Default: 5]\n");
  fprintf(stderr,"\t-d delay [Default: 1]\n");
  fprintf(stderr,"\t-q dimension to project to [Default: 3]\n");
  fprintf(stderr,"\t-k minimal number of neighbours [Default: 30]\n");
  fprintf(stderr,"\t-r minimal neighbourhood size \n\t\t"
	  "[Default: (interval of data)/1000]\n");
  fprintf(stderr,"\t-i # of iterations [Default: 1]\n");
  fprintf(stderr,"\t-2 use euklidean metric [Default: non euklidean]\n");
  fprintf(stderr,"\t-o name of output file \n\t\t"
	  "[Default: 'datafile'.opt.n, where n is the iteration.\n\t\t"
	  " If no -o is given, the last iteration is also"
	  " written to stdout]\n");
  fprintf(stderr,"\t-V verbosity level [Default: 7]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n\t\t"
          "2='+ average correction and trend'\n\t\t"
	  "4='+ how many points for which epsilon'\n");
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
  if ((out=check_option(in,n,'q','u')) != NULL)
    sscanf(out,"%u",&qdim);
  if ((out=check_option(in,n,'k','u')) != NULL)
    sscanf(out,"%u",&minn);
  if ((out=check_option(in,n,'r','f')) != NULL) {
    eps_set=1;
    sscanf(out,"%lf",&mineps);
  }
  if ((out=check_option(in,n,'i','u')) != NULL)
    sscanf(out,"%u",&iterations);
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'2','n')) != NULL)
    euclidean=1;
  if ((out=check_option(in,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

void sort(double *x,int *n)
{
  int i,j,iswap;
  double dswap;
  
  for (i=0;i<dim;i++)
    n[i]=i;
  
  for (i=0;i<dim-1;i++)
    for (j=i+1;j<dim;j++)
      if (x[j] > x[i]) {
	dswap=x[i];
	x[i]=x[j];
	x[j]=dswap;
	iswap=n[i];
	n[i]=n[j];
	n[j]=iswap;
      }
}

void make_correction(unsigned long n,unsigned long nf)
{
  int i,i1,j,j1,k,hs;
  double help;
  
  for (i=0;i<dim;i++) {
    i1=i*delay;
    help=0.0;
    for (j=0;j<nf;j++)
      help += series[flist[j]-i1];
    av[i]=help/nf;
  }

  for (i=0;i<dim;i++) {
    i1=i*delay;
    for (j=i;j<dim;j++) {
      help=0.0;
      j1=j*delay;
      for (k=0;k<nf;k++) {
	hs=flist[k];
	help += series[hs-i1]*series[hs-j1];
      }
      mat[i][j]=(help/nf-av[i]*av[j])*metric[i]*metric[j];
      mat[j][i]=mat[i][j];
    }
  }

  eig1(mat,(long)dim,eig,off); 
  eig2(eig,off,(long)dim,mat);
  sort(eig,sorted);

  for (i=0;i<dim;i++) {
    help=0.0;
    for (j=qdim;j<dim;j++) {
      hs=sorted[j];
      for (k=0;k<dim;k++) {
	help += (series[n-k*delay]-av[k])*mat[k][hs]*mat[i][hs]*metric[k];
      }
    }
    corr[n][i]=help/metric[i];
  }
}

void handle_trend(unsigned long n,unsigned long nf)
{
  int i,j;
  double *av,help;
  
  check_alloc(av=(double*)malloc(sizeof(double)*dim));
  
  for (i=0;i<dim;i++) {
    help=0.0;
    for (j=0;j<nf;j++)
      help += corr[flist[j]][i];
    av[i]=help/nf;
  }
  
  for (i=0;i<dim;i++)
    delta[n-i*delay] += (corr[n][i]-av[i])/(trace*metric[i]);
  free(av);
}

void set_correction(void)
{
  int i;
  unsigned long toolarge=0;
  double av=0.0,sigma=0.0,help,hhelp;

  for (i=0;i<length;i++) {
    av += (help=delta[i]);
    sigma += help*help;
  }
  av /= length;
  sigma=sqrt(fabs(sigma/length-av*av));
  if (verbosity&(VER_USR1|VER_USR2)) {
    fprintf(stderr,"Average shift of the data= %e\n",av*d_max);
    fprintf(stderr,"Average rms correction= %e\n\n",sigma*d_max);
  }
  for (i=0;i<length;i++) {
    help=delta[i];
    if ((hhelp=fabs(help)) < 10.0*sigma)
      series[i] -= help;
    else {
      series[i] -= help*exp(-fabs(hhelp)/sigma);
      toolarge++;
    }
  }
  if (verbosity&(VER_USR1|VER_USR2))
    fprintf(stderr,"For %ld points the correction was unreasonably large\n",
	    toolarge);
  if (resize_eps) {
    mineps /= epsfac;
    if (verbosity&VER_USR2)
      fprintf(stderr,"Reset minimal neighbourhood size to %e\n",mineps*d_max);
  }
  resize_eps=0;
}

int main(int argc,char **argv)
{
  char stdi=0;
  int iter,i,j,epscount,*ok;
  char all_done;
  char *ofname;
  unsigned long nfound,n,allfound;
  double epsilon;
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
      sprintf(outfile,"%s.opt",infile);
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      check_alloc(ofname=(char*)calloc((size_t)14,(size_t)1));
      sprintf(outfile,"stdin.opt");
    }
  }
  else
    check_alloc(ofname=(char*)calloc(strlen(outfile)+10,(size_t)1));
  
  series=(double*)get_series(infile,&length,exclude,column,verbosity);
  if (length < minn) {
    fprintf(stderr,"With %lu data you will never find %u neighbors."
	    " Exiting!\n",length,minn);
    exit(GHKSS__TOO_MANY_NEIGHBORS);
  }
  rescale_data(series,length,&d_min,&d_max);

  if (!eps_set)
    mineps=1./1000.;
  else
    mineps /= d_max;
  epsfac=sqrt(2.0);

  check_alloc(box=(long**)malloc(sizeof(long*)*BOX));
  for (i=0;i<BOX;i++)
    check_alloc(box[i]=(long*)malloc(sizeof(long)*BOX));

  check_alloc(list=(long*)malloc(sizeof(long)*length));
  check_alloc(flist=(unsigned long*)malloc(sizeof(long)*length));

  check_alloc(metric=(double*)malloc(sizeof(double)*dim));
  trace=0.0;
  for (i=1;i<dim-1;i++) {
    metric[i]=1.0;
    trace += 1./metric[i];
  }
  if (!euclidean)
    metric[0]=metric[dim-1]=1.0e3;
  else
    metric[0]=metric[dim-1]=1.0;
  trace += (1./metric[0]+1./metric[dim-1]);

  check_alloc(corr=(double**)malloc(sizeof(double*)*length));
  for (i=0;i<length;i++)
    check_alloc(corr[i]=(double*)malloc(sizeof(double)*dim));
  check_alloc(delta=(double*)malloc(sizeof(double)*length));
  check_alloc(ok=(int*)malloc(sizeof(int)*length));

  check_alloc(av=(double*)malloc(sizeof(double)*dim));
  check_alloc(sorted=(int*)malloc(sizeof(int)*dim));
  check_alloc(eig=(double*)malloc(sizeof(double)*dim));
  check_alloc(off=(double*)malloc(sizeof(double)*dim));
  check_alloc(mat=(double**)malloc(sizeof(double*)*dim));
  for (i=0;i<dim;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*dim));


  resize_eps=0;
  for (iter=1;iter<=iterations;iter++) {
    for (i=0;i<length;i++) {
      ok[i]=0;
      delta[i]=0.0;
      for (j=0;j<dim;j++)
	corr[i][j]=0.0;
    }
    epsilon=mineps;
    all_done=0;
    epscount=1;
    allfound=0;
    if (verbosity&(VER_USR1|VER_USR2))
      fprintf(stderr,"Starting iteration %d\n",iter);
    while(!all_done) {
      make_box(series,box,list,length,BOX,dim,delay,epsilon);
      all_done=1;
      for (n=(dim-1)*delay;n<length;n++)
	if (!ok[n]) {
	  nfound=find_neighbors(series,box,list,series+n,length,BOX,dim,delay,
				epsilon,flist);
	  if (nfound >= minn) {
	    make_correction(n,nfound);
	    ok[n]=epscount;
	    if (epscount == 1)
	      resize_eps=1;
	    allfound++;
	  }
	  else
	    all_done=0;
	}
      if (verbosity&VER_USR2)
	fprintf(stderr,"Corrected %ld points with epsilon= %e\n",allfound,
		epsilon*d_max);
      epsilon *= epsfac;
      epscount++;
    }
    if (verbosity&VER_USR2)
      fprintf(stderr,"Start evaluating the trend\n");

    epsilon=mineps;
    allfound=0;
    for (i=1;i<epscount;i++) {
      make_box(series,box,list,length,BOX,dim,delay,epsilon);
      for (n=(dim-1)*delay;n<length;n++)
	if (ok[n] == i) {
	  nfound=find_neighbors(series,box,list,series+n,length,BOX,dim,delay,
				epsilon,flist);
	  handle_trend(n,nfound);
	  allfound++;
	}
      if (verbosity&VER_USR2)
	fprintf(stderr,"Trend subtracted for %ld points with epsilon= %e\n",
		allfound,epsilon*d_max);
      epsilon *= epsfac;
    }
    set_correction();
    
    sprintf(ofname,"%s.%d",outfile,iter);
    test_outfile(ofname);

    file=fopen(ofname,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n\n",ofname);
    for (i=0;i<length;i++) {
      fprintf(file,"%e\n",series[i]*d_max+d_min);
      if (stdo && (iter == iterations))
	fprintf(stdout,"%e\n",series[i]*d_max+d_min);
    }
    fclose(file);
  }

  return 0;
}
