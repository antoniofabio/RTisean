#ifndef _NS_ONESTEP
#define _NS_ONESTEP

struct ns_onestep{


char stdo;
char* outfile;
unsigned int nmax;
long **box,*list;
unsigned long *found;
double *series;
double interval,min,epsilon;

char epsset,causalset;
unsigned int COLUMN;
unsigned int verbosity;
int DIM,DELAY,MINN,STEP;
double EPS0,EPSF;
unsigned long LENGTH,exclude,CLENGTH,causal;
char *infile;

void show_options(char *progname);

void scan_options(int n,char **in);

double make_fit(long act,unsigned long number);

int main(int argc,char **argv);
};

#endif

