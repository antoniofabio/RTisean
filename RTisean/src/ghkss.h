#ifndef _NS_GHKSS
#define _NS_GHKSS


struct ns_ghkss{

unsigned long length,exclude;
unsigned int dim,qdim,delay,column,minn,iterations;
unsigned int verbosity;
double mineps,epsfac;
char eps_set,euclidean,resize_eps;
char *outfile,stdo;
char *infile;

double d_max,d_min;
double *series,*delta,**corr;
double *metric,trace;
long **box,*list;
unsigned long *flist;



int *sorted;
double *av,**mat,*eig,*off;


void show_options(char *progname);

void scan_options(int n,char **in);

void sort(double *x,int *n);

void make_correction(unsigned long n,unsigned long nf);

void handle_trend(unsigned long n,unsigned long nf);

void set_correction(void);

int main(int argc,char **argv);
};

#endif


