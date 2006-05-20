#ifndef _NS_XZERO
#define _NS_XZERO


struct ns_xzero{

unsigned int nmax;
long **box,*list;
unsigned long *found;
double *series1,*series2;
double interval,min,epsilon;

char epsset;
char *infile;
char *outfile,stdo;
char *COLUMNS;
unsigned int DIM,DELAY;
unsigned int verbosity;
int MINN,STEP;
double EPS0,EPSF;
unsigned long LENGTH,exclude,CLENGTH;

void show_options(char *progname);

void scan_options(int n,char **in);

double make_fit(unsigned long act,unsigned long number,unsigned long istep);

int main(int argc,char **argv);
};

#endif


