/*Author: Rainer Hegger, last modified May 25, 2000  */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include <string.h>
#include "tsa.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

#include "lyap_spec.h"

#define WID_STR "Estimates the spectrum of Lyapunov exponents using the\n\t\
method of Sano and Sawada."

#define OUT 10

#define BOX 512
#define EPSMAX 1.0







void ns_lyap_spec::show_options(char *progname)
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
  fprintf(stderr,"\t-m # of components,embedding dimension [default %d,%d]\n",
	  DIMENSION,EMBED);
  fprintf(stderr,"\t-d delay  [default %d]\n",DELAY);
  fprintf(stderr,"\t-r epsilon size to start with [default "
  "(data interval)/1000]\n");
  fprintf(stderr,"\t-f factor to increase epsilon [default: 1.2]\n");
  fprintf(stderr,"\t-k # of neighbors to use [default: 30]\n");
  fprintf(stderr,"\t-n # of iterations [default: length]\n");
  fprintf(stderr,"\t-I invert the time series [default: no]\n");
  fprintf(stderr,"\t-o name of output file [default 'datafile'.lyaps]\n");
  fprintf(stderr,"\t-V verbosity level [default: 1]\n\t\t"
          "0='only panic messages'\n\t\t"
          "1='+ input/output messages'\n");
  fprintf(stderr,"\t-h show these options\n");
  fprintf(stderr,"\n");
  exit(0);
}

void ns_lyap_spec::scan_options(int n,char **argv)
{
  char *out;
  
  if ((out=check_option(argv,n,'l','u')) != NULL)
    sscanf(out,"%lu",&LENGTH);
  if ((out=check_option(argv,n,'x','u')) != NULL)
    sscanf(out,"%lu",&exclude);
  if ((out=check_option(argv,n,'c','s')) != NULL)
    COLUMNS=out;
  if ((out=check_option(argv,n,'d','u')) != NULL)
    sscanf(out,"%u",&DELAY);
  if ((out=check_option(argv,n,'m','2')) != NULL) {
    sscanf(out,"%u,%u",&DIMENSION,&EMBED);
    dimset=1;
  }
  if ((out=check_option(argv,n,'n','u')) != NULL)
    sscanf(out,"%lu",&ITERATIONS);
  if ((out=check_option(argv,n,'r','f')) != NULL) {
    epsset=1;
    sscanf(out,"%lf",&epsmin);
  }
  if ((out=check_option(argv,n,'f','f')) != NULL)
    sscanf(out,"%lf",&EPSSTEP);
  if ((out=check_option(argv,n,'k','u')) != NULL)
    sscanf(out,"%u",&MINNEIGHBORS);
  if ((out=check_option(argv,n,'V','u')) != NULL)
    sscanf(out,"%u",&verbosity);
  if ((out=check_option(argv,n,'I','n')) != NULL)
    INVERSE=1;
  if ((out=check_option(argv,n,'o','o')) != NULL) {
    stdo=0;
    if (strlen(out) > 0)
      outfile=out;
  }
}

double ns_lyap_spec::sort(long act,unsigned long nfound)
{
  double maxeps=0.0,dx,*abstand,dswap;
  int i,j,del,hf,iswap,n1;

  check_alloc(abstand=(double*)malloc(nfound*sizeof(double)));

  for (i=0;i<nfound;i++) {
    hf=found[i];
    if (hf != act) {
      abstand[i]=fabs(series[0][act]-series[0][hf]);
      for (j=1;j<alldim;j++) {
	del=indexes[j][0];
	n1=indexes[j][1];
	dx=fabs(series[n1][act-del]-series[n1][hf-del]);
	if (dx > abstand[i]) abstand[i]=dx;
      }
    }
    else
      abstand[i]=2.0*EPSMAX;
  }
  for (i=0;i<nfound-1;i++)
    for (j=i+1;j<nfound;j++)
      if (abstand[j]<abstand[i]) {
	dswap=abstand[i];
	abstand[i]=abstand[j];
	abstand[j]=dswap;
	iswap=found[i];
	found[i]=found[j];
	found[j]=iswap;
      }

  maxeps=abstand[MINNEIGHBORS-1];
  free(abstand);

  return maxeps;
}

void ns_lyap_spec::make_dynamics(double **dynamics,long act)
{
  long i,hi,j,hj,k,t=act,d;
  unsigned long nfound=0;
  double **hser,**imat;
  double epsilon,hv,hv1;
  double ynew;
  
  check_alloc(hser=(double**)malloc(sizeof(double*)*DIMENSION));
  for (i=0;i<DIMENSION;i++)
    hser[i]=series[i]+act;
  for (epsilon=epsmin;epsilon<=EPSMAX;epsilon*=EPSSTEP) {
    make_multi_box(series,box,list,LENGTH-DELAY,BOX,DIMENSION,EMBED,
		   DELAY,epsilon);
    nfound=find_multi_neighbors(series,box,list,hser,LENGTH-DELAY,BOX,
				DIMENSION,EMBED,DELAY,epsilon,found);
    if (nfound > MINNEIGHBORS) {
      epsilon=sort(act,nfound);
      nfound=MINNEIGHBORS;
      break;
    }
  }
  free(hser);

  avneig += nfound;
  aveps += epsilon;
  epsmin=aveps/count;
  if (nfound < MINNEIGHBORS) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"#Not enough neighbors found. Exiting\n");
    exit(LYAP_SPEC_NOT_ENOUGH_NEIGHBORS);
  }
  
  for (i=0;i<=alldim;i++) {
    vec[i]=0.0;
    for (j=0;j<=alldim;j++) 
      mat[i][j]=0.0;
  }
  
  for (i=0;i<nfound;i++) {
    act=found[i];
    mat[0][0] += 1.0;
    for (j=0;j<alldim;j++)
      mat[0][j+1] += series[indexes[j][1]][act-indexes[j][0]];
    for (j=0;j<alldim;j++) {
      hv1=series[indexes[j][1]][act-indexes[j][0]];
      hj=j+1;
      for (k=j;k<alldim;k++)
	mat[hj][k+1] += series[indexes[k][1]][act-indexes[k][0]]*hv1;
    }
  }

  for (i=0;i<=alldim;i++)
    for (j=i;j<=alldim;j++)
      mat[j][i]=(mat[i][j]/=(double)nfound);
  
  imat=invert_matrix(mat,alldim+1);
  
  for (d=0;d<DIMENSION;d++) {
    for (i=0;i<=alldim;i++)
      vec[i]=0.0;
    for (i=0;i<nfound;i++) {
      act=found[i];
      hv=series[d][act+DELAY];
      vec[0] += hv;
      for (j=0;j<alldim;j++)
	vec[j+1] += hv*series[indexes[j][1]][act-indexes[j][0]];
    }
    for (i=0;i<=alldim;i++)
      vec[i] /= (double)nfound;
    
    ynew=0.0;
    for (i=0;i<=alldim;i++)
      ynew += imat[0][i]*vec[i];
    for (i=1;i<=alldim;i++) {
      hi=i-1;
      dynamics[d][hi]=0.0;
      for (j=0;j<=alldim;j++)
	dynamics[d][hi] += imat[i][j]*vec[j];
    }
    for (i=0;i<alldim;i++)
      ynew += dynamics[d][i]*series[indexes[i][1]][t-indexes[i][0]];
    averr[d] += (ynew-series[d][t+DELAY])*(ynew-series[d][t+DELAY]);
  }

  for (i=0;i<=alldim;i++)
    free(imat[i]);
  free(imat);
}

void ns_lyap_spec::gram_schmidt(double **delta,
		  double *stretch)
{
  double **dnew,norm,*diff;
  long i,j,k;
  
  check_alloc(diff=(double*)malloc(sizeof(double)*alldim));
  check_alloc(dnew=(double**)malloc(sizeof(double*)*alldim));
  for (i=0;i<alldim;i++)
    check_alloc(dnew[i]=(double*)malloc(sizeof(double)*alldim));

  for (i=0;i<alldim;i++) {
    for (j=0;j<alldim;j++) 
      diff[j]=0.0;
    for (j=0;j<i;j++) {
      norm=0.0;
      for (k=0;k<alldim;k++)
	norm += delta[i][k]*dnew[j][k];
      for (k=0;k<alldim;k++)
	diff[k] -= norm*dnew[j][k];
    }
    norm=0.0;
    for (j=0;j<alldim;j++)
      norm += sqr(delta[i][j]+diff[j]);
    stretch[i]=(norm=sqrt(norm));
    for (j=0;j<alldim;j++)
      dnew[i][j]=(delta[i][j]+diff[j])/norm;
  }
  for (i=0;i<alldim;i++)
    for (j=0;j<alldim;j++)
      delta[i][j]=dnew[i][j];

  free(diff);
  for (i=0;i<alldim;i++)
    free(dnew[i]);
  free(dnew);
}

void ns_lyap_spec::make_iteration(double **dynamics,
		    double **delta)
{
  double **dnew;
  long i,j,k;

  check_alloc(dnew=(double**)malloc(sizeof(double*)*alldim));
  for (i=0;i<alldim;i++)
    check_alloc(dnew[i]=(double*)malloc(sizeof(double)*alldim));

  for (i=0;i<alldim;i++) {
    for (j=0;j<DIMENSION;j++) {
      dnew[i][j]=dynamics[j][0]*delta[i][0];
      for (k=1;k<alldim;k++)
	dnew[i][j] += dynamics[j][k]*delta[i][k];
    }
    for (j=DIMENSION;j<alldim;j++)
      dnew[i][j]=delta[i][j-1];
  }

  for (i=0;i<alldim;i++)
    for (j=0;j<alldim;j++)
      delta[i][j]=dnew[i][j];

  for (i=0;i<alldim;i++)
    free(dnew[i]);
  free(dnew);
}

int ns_lyap_spec::main(int argc,char **argv)
{

    epsset=0;stdo=1;
    outfile=NULL;
    infile=NULL;
    dimset=0;
    COLUMNS=NULL;
    LENGTH=ULONG_MAX;exclude=0;
    EMBED=2;DIMENSION=1;DELAY=1;MINNEIGHBORS=30;
    verbosity=0xff;
    EPSSTEP=1.2;

    avneig=0.0;aveps=0.0;
    imax=BOX-1;count=0;


  char stdi=0;
  double **delta,**dynamics,*lfactor;
  double *factor,dim;
  double *hseries;
  double *interval,*min,*av,*var,maxinterval;
  long start,i,j;
  time_t lasttime,newtime;
  FILE *file=NULL;

  if (scan_help(argc,argv))
    show_options(argv[0]);

  ITERATIONS=ULONG_MAX;
  
  scan_options(argc,argv);
#ifndef OMIT_WHAT_I_DO
  if (verbosity&VER_INPUT)
    what_i_do(argv[0],WID_STR);
#endif

  infile=search_datafile(argc,argv,NULL,verbosity);
  if (infile == NULL)
    stdi=1;

  if (outfile == NULL) {
    if (!stdi) {
      check_alloc(outfile=(char*)calloc(strlen(infile)+7,(size_t)1));
      strcpy(outfile,infile);
      strcat(outfile,".lyaps");
    }
    else {
      check_alloc(outfile=(char*)calloc((size_t)12,(size_t)1));
      strcpy(outfile,"stdin.lyaps");
    }
  }
  if (!stdo)
    test_outfile(outfile);

  alldim=DIMENSION*EMBED;

  if (COLUMNS == NULL)
    series=(double**)get_multi_series(infile,&LENGTH,exclude,&DIMENSION,"",
				      dimset,verbosity);
  else
    series=(double**)get_multi_series(infile,&LENGTH,exclude,&DIMENSION,
				      COLUMNS,dimset,verbosity);

  if (MINNEIGHBORS > (LENGTH-DELAY*(EMBED-1)-1)) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Your time series is not long enough to find %d neighbors!"
	    " Exiting.\n",MINNEIGHBORS);
    exit(LYAP_SPEC_DATA_TOO_SHORT);
  }

  check_alloc(min=(double*)malloc(sizeof(double)*DIMENSION));
  check_alloc(interval=(double*)malloc(sizeof(double)*DIMENSION));
  check_alloc(av=(double*)malloc(sizeof(double)*DIMENSION));
  check_alloc(var=(double*)malloc(sizeof(double)*DIMENSION));
  check_alloc(averr=(double*)malloc(sizeof(double)*DIMENSION));
  maxinterval=0.0;
  for (i=0;i<DIMENSION;i++) {
    averr[i]=0.0;
    rescale_data(series[i],LENGTH,&min[i],&interval[i]);
    if (interval[i] > maxinterval) 
      maxinterval=interval[i];
    variance(series[i],LENGTH,&av[i],&var[i]);
  }
  
  if (INVERSE) {
    check_alloc(hseries=(double*)malloc(sizeof(double)*LENGTH));
    for (j=0;j<DIMENSION;j++) {
      for (i=0;i<LENGTH;i++)
	hseries[LENGTH-1-i]=series[j][i];
      for (i=0;i<LENGTH;i++)
	series[j][i]=hseries[i];
    }
    free(hseries);
  }
  
  if (!epsset)
    epsmin=1./1000.;
  else
    epsmin /= maxinterval;
  
  check_alloc(box=(long**)malloc(sizeof(long*)*BOX));
  for (i=0;i<BOX;i++)
    check_alloc(box[i]=(long*)malloc(sizeof(long)*BOX));

  check_alloc(list=(long*)malloc(sizeof(long)*LENGTH));
  check_alloc(found=(unsigned long*)malloc(sizeof(long)*LENGTH));

  check_alloc(dynamics=(double**)malloc(sizeof(double*)*DIMENSION));
  for (i=0;i<DIMENSION;i++)
    check_alloc(dynamics[i]=(double*)malloc(sizeof(double)*alldim));
  check_alloc(factor=(double*)malloc(sizeof(double)*alldim));
  check_alloc(lfactor=(double*)malloc(sizeof(double)*alldim));
  check_alloc(delta=(double**)malloc(sizeof(double*)*alldim));
  for (i=0;i<alldim;i++)
    check_alloc(delta[i]=(double*)malloc(sizeof(double)*alldim));
  
  check_alloc(vec=(double*)malloc(sizeof(double)*(alldim+1)));
  check_alloc(mat=(double**)malloc(sizeof(double*)*(alldim+1)));
  for (i=0;i<=alldim;i++)//fixed mem allocation error
    check_alloc(mat[i]=(double*)malloc(sizeof(double)*(alldim+1)));
  
  check_alloc(indexes=(unsigned int**)malloc(sizeof(int*)*alldim));
  for (i=0;i<alldim;i++) {
    check_alloc(indexes[i]=(unsigned int*)malloc(sizeof(int)*2));
    indexes[i][0]=(unsigned int)(i/DIMENSION)*DELAY;
    indexes[i][1]=i%DIMENSION;
  }

  rnd_init(0x098342L);
  for (i=0;i<10000;i++)
    rnd_long();
  for (i=0;i<alldim;i++) {
    factor[i]=0.0;
    for (j=0;j<alldim;j++)
      delta[i][j]=(double)rnd_long()/(double)ULONG_MAX;
      //if (i<=j) delta[i][j]=1.0;
      //else delta[i][j]=0.0;
  }





  gram_schmidt(delta,lfactor);
  


  start=ITERATIONS;
  if (start>(LENGTH-DELAY)) 
    start=LENGTH-DELAY;

  if (!stdo) {
    file=fopen(outfile,"w");
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Opened %s for writing\n",outfile);
  }
  else {
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Writing to stdout\n");
  }

  time(&lasttime);
  for (i=(EMBED-1)*DELAY;i<start;i++) {
    count++;
    make_dynamics(dynamics,i);
    make_iteration(dynamics,delta);
    gram_schmidt(delta,lfactor);
    for (j=0;j<alldim;j++) {
      factor[j] += log(lfactor[j])/(double)DELAY;
    }
    if (((time(&newtime)-lasttime) > OUT) || (i == (start-1))) {
      time(&lasttime);
      if (!stdo) {
	fprintf(file,"%ld ",count);
	for (j=0;j<alldim;j++) 
	  fprintf(file,"%e ",factor[j]/count);
	fprintf(file,"\n");
	fflush(file);
      }
      else {
	fprintf(stdout,"%ld ",count);
	for (j=0;j<alldim;j++) 
	  fprintf(stdout,"%e ",factor[j]/count);
	fprintf(stdout,"\n");
      }
    }
  }
  
  dim=0.0;
  for (i=0;i<alldim;i++) {
    dim += factor[i];
    if (dim < 0.0)
      break;
  }
  if (i < alldim)
    dim=i+(dim-factor[i])/fabs(factor[i]);
  else
    dim=alldim;
  if (!stdo) {
    fprintf(file,"#Average relative forecast errors:= ");
    for (i=0;i<DIMENSION;i++)
      fprintf(file,"%e ",sqrt(averr[i]/count)/var[i]);
    fprintf(file,"\n");
    fprintf(file,"#Average Neighborhood Size= %e\n",aveps*maxinterval/count);
    fprintf(file,"#estimated KY-Dimension= %f\n",dim);
  }
  else {
    fprintf(stdout,"#Average relative forecast errors:= ");
    for (i=0;i<DIMENSION;i++)
      fprintf(stdout,"%e ",sqrt(averr[i]/count)/var[i]);
    fprintf(stdout,"\n");
    fprintf(stdout,"#Average Neighborhood Size= %e\n",aveps*maxinterval/count);
    fprintf(stdout,"#estimated KY-Dimension= %f\n",dim);
  }
  if (!stdo)
    fclose(file);

  return 0;
}



