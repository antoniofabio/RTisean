/*Author: Rainer Hegger, last modified Sep 4, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "polynom.h"


#define WID_STR "Fits a polynomial to the data"



void ns_polynom::show_options(char *progname)
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
  fprintf(stderr,"\t-p order of the polynomial [default: 2]\n");
  fprintf(stderr,"\t-n # of points for insample [default: # of data]\n");
  fprintf(stderr,"\t-L steps to cast [default: none]\n");
  fprintf(stderr,"\t-o output file name [default: 'datafile'.pol]\n");
  fprintf(stderr,"\t-V verbosity level [default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  exit(0);
}

void ns_polynom::scan_options(int n,char **in)
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
  if ((out=check_option(in,n,'p','u')) != NULL)
    sscanf(out,"%u",&N);
  if ((out=check_option(in,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(in,n,'n','u')) != NULL) {
    sscanf(out,"%lu",&INSAMPLE);
    sinsample=1;
  }
  if ((out=check_option(in,n,'L','u')) != NULL) {
    CAST=1;
    sscanf(out,"%lu",&CLENGTH);
  }
  if ((out=check_option(in,n,'o','o')) != NULL)
    if (strlen(out) > 0)
      outfile=out;
}

double ns_polynom::polynom(int act,int dim,long cur,long fac)
{
  int j,n,hi;
  double ret=1.0;

  n=cur/fac;
  hi=act-(dim-1)*DELAY;
  for (j=1;j<=n;j++)
    ret *= series[hi];
  if (dim > 1) 
    ret *= polynom(act,dim-1,cur-n*fac,fac/(N+1));

  return ret;
}

int ns_polynom::number_pars(int ord,int start)
{
  int i,ret=0;

  if (ord == 1)
    for (i=start;i<=DIM;i++)
      ret += 1;
  else
    for (i=start;i<=DIM;i++)
      ret += number_pars(ord-1,i);

  return ret;
}

void ns_polynom::make_coding(int ord,int d,int fac,int cur)
{
  int j;

  if ( d == -1)
    coding[hpar++]=cur;
  else
    for (j=0;j<=ord;j++)
      make_coding(ord-j,d-1,fac*(N+1),cur+j*fac);
}

void ns_polynom::make_fit(void)
{
  int i,j,k;
  double **mat,*b;
  
  check_alloc(b=(double*)malloc(sizeof(double)*pars));
  check_alloc(mat=(double**)malloc(sizeof(double*)*pars));
  for (i=0;i<pars;i++)
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*pars));

  for (i=0;i<pars;i++) {
    b[i]=0.0;
    for (j=0;j<pars;j++)
      mat[i][j]=0.0;
  }

  for (i=0;i<pars;i++)
    for (j=i;j<pars;j++)
      for (k=(DIM-1)*DELAY;k<INSAMPLE-1;k++)
	mat[i][j] += polynom(k,DIM,coding[i],maxencode)*
	  polynom(k,DIM,coding[j],maxencode);
  for (i=0;i<pars;i++)
    for (j=i;j<pars;j++)
      mat[j][i]=(mat[i][j] /= (INSAMPLE-1-(DIM-1)*DELAY));

  for (i=0;i<pars;i++) {
    for (j=(DIM-1)*DELAY;j<INSAMPLE-1;j++)
      b[i] += series[j+1]*polynom(j,DIM,coding[i],maxencode);
    b[i] /= (INSAMPLE-1-(DIM-1)*DELAY);
  }
  solvele(mat,b,pars);

  for (i=0;i<pars;i++)
    results[i]=b[i];
  
  free(b);
  for (i=0;i<pars;i++)
    free(mat[i]);
  free(mat);
}

void ns_polynom::decode(int *out,int dim,long cur,long fac)
{
  int n;
  
  n=cur/fac;
  out[dim]=n;
  if (dim > 0) 
    decode(out,dim-1,cur-(long)n*fac,fac/(N+1));
}

double ns_polynom::make_error(unsigned long i0,unsigned long i1)
{
  int j,k;
  double h,err;
  
  err=0.0;
  for (j=i0+(DIM-1)*DELAY;j<(long)i1-1;j++) {
    h=0.0;
    for (k=0;k<pars;k++) 
      h += results[k]*polynom(j,DIM,coding[k],maxencode);
    err += (series[j+1]-h)*(series[j+1]-h);
  }
  return err /= ((long)i1-(long)i0-(DIM-1)*DELAY);
}

void ns_polynom::make_cast(FILE *fcast)
{
  int i,j,k,hi;
  double casted;
  
  for (i=0;i<=(DIM-1)*DELAY;i++)
    series[i]=series[LENGTH-(DIM-1)*DELAY-1+i];

  hi=(DIM-1)*DELAY;
  for (i=1;i<=CLENGTH;i++) {
    casted=0.0;
    for (k=0;k<pars;k++)
      casted += results[k]*polynom((DIM-1)*DELAY,DIM,coding[k],maxencode);
    fprintf(fcast,"%e\n",casted*varianz);
    fflush(fcast);
    for (j=0;j<(DIM-1)*DELAY;j++)
      series[j]=series[j+1];
    series[hi]=casted;
  }
  fclose(fcast);
}

int ns_polynom::main(int argc,char **argv)
{

CAST=0;sinsample=0;outfile=NULL;
infile=NULL;
LENGTH=ULONG_MAX;exclude=0;
CLENGTH=1000;
INSAMPLE=ULONG_MAX;
DIM=2;DELAY=1;N=2;
COLUMN=1;
pars=1;
verbosity=0xff;



  char stdi=0;
  int i,j,k;
  int *opar,sumpar;
  double in_error,out_error,av;
  FILE *file;

  if (scan_help(argc,argv))
    show_options(argv[0]);

  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  infile=search_datafile(argc,argv,&COLUMN,verbosity);
  if (infile == NULL)
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+5,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".pol");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)10,(size_t)1));
      strcpy(outfile,"stdin.pol");
    }
  }
  test_outfile(outfile);

  series=(double*)get_series(infile,&LENGTH,exclude,COLUMN,verbosity);
  variance(series,LENGTH,&av,&varianz);
  for (i=0;i<LENGTH;i++)
    series[i] /= varianz;

  if (!sinsample || (INSAMPLE > LENGTH))
    INSAMPLE=LENGTH;

  maxencode=1;
  for (i=1;i<DIM;i++)
    maxencode *= (N+1);
  
  for (i=1;i<=N;i++) {
    pars += number_pars(i,1);
  }
  file=fopen(outfile,"w");
  if (verbosity&VER_INPUT)
    fprintf(stderr,"Opened %s for writing\n",outfile);
  fprintf(file,"#number of free parameters= %d\n\n",pars);
  fflush(file);
  check_alloc(coding=(long*)malloc(sizeof(long)*pars));
  hpar=0;
  make_coding(N,DIM-1,1,0);

  check_alloc(results=(double*)malloc(sizeof(double)*pars));
  make_fit();

  check_alloc(opar=(int*)malloc(sizeof(int)*DIM));
  fprintf(file,"#used norm for the fit= %e\n",varianz);

  for (j=0;j<pars;j++) {
    decode(opar,DIM-1,coding[j],maxencode);
    fprintf(file,"#");
    sumpar=0;
    for (k=0;k<DIM;k++) {
      sumpar += opar[k];
      fprintf(file,"%d ",opar[k]);
    }
    fprintf(file,"%e\n",results[j]/pow(varianz,(double)(sumpar-1)));
  }
  fprintf(file,"\n");

  in_error=make_error((unsigned long)0,INSAMPLE);

  fprintf(file,"#average insample error= %e\n",sqrt(in_error/varianz));

  if (INSAMPLE < LENGTH) {
    out_error=make_error(INSAMPLE,LENGTH);
    fprintf(file,"#average out of sample error= %e\n",sqrt(out_error)/varianz);
  }

  if (CAST)
    make_cast(file);
  fclose(file);

  return 0;
}

