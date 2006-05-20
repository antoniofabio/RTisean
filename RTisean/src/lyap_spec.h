#ifndef _LYAP_SPEC
#define _LYAP_SPEC

struct ns_lyap_spec{

char epsset,stdo;
char INVERSE,*outfile;
char *infile;
char dimset;
char *COLUMNS;
unsigned long LENGTH,ITERATIONS,exclude;
unsigned int EMBED,DIMENSION,DELAY,MINNEIGHBORS;
unsigned int verbosity;
double EPSSTEP;

double **series,*averr,avneig,aveps;
double **mat,*vec;
double epsmin;
long imax,count;
long **box,*list;
unsigned long *found;
unsigned int alldim,**indexes;

void show_options(char *progname);

void scan_options(int n,char **argv);

double sort(long act,unsigned long nfound);

void make_dynamics(double **dynamics,long act);

void gram_schmidt(double **delta,
		  double *stretch);

void make_iteration(double **dynamics,
		    double **delta);

int main(int argc,char **argv);
};

#endif

