#ifndef _NS_POLYNOM
#define _NS_POLYNOM


struct ns_polynom{

char CAST,sinsample,*outfile;
char *infile;
unsigned long LENGTH,exclude;
long CLENGTH;
unsigned long INSAMPLE;
int DIM,DELAY,N;
unsigned int COLUMN;
unsigned int pars,hpar;
unsigned int verbosity;

long *coding;
long maxencode;
double *series,*results;
double varianz;

void show_options(char *progname);

void scan_options(int n,char **in);

double polynom(int act,int dim,long cur,long fac);

int number_pars(int ord,int start);

void make_coding(int ord,int d,int fac,int cur);

void make_fit(void);

void decode(int *out,int dim,long cur,long fac);

double make_error(unsigned long i0,unsigned long i1);

void make_cast(FILE *fcast);

int main(int argc,char **argv);
};

#endif

