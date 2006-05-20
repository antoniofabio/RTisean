#ifndef _NS_LL_AR
#define _NS_LL_AR

struct ns_ll_ar{

unsigned int nmax;
long **box,*list;
unsigned long *found;
double *series;

char eps0set,eps1set,causalset;
char *outfile,stdo;
unsigned int COLUMN;
unsigned int DIM,DELAY;
unsigned int verbosity;
int STEP;
double EPS0,EPS1,EPSF;
unsigned long LENGTH,exclude,CLENGTH,causal;
char *infile;
double **mat,*vec;

void show_options(char *progname);

void scan_options(int n,char **in);

double make_fit(long act,unsigned long number);

int main(int argc,char **argv);
};

#endif


