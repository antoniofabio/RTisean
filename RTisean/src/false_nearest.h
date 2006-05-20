#ifndef _NS_FALSE_NEAREST
#define _NS_FALSE_NEAREST

struct ns_false_nearest{

char *outfile;
char *infile;
char stdo;
unsigned long length,exclude,theiler;
unsigned int column,delay,maxdim,mindim;
unsigned int verbosity;
double rt;
double eps0;
double *series;
double aveps,vareps;
double varianz;

int ibox;
long **box,*list;
unsigned long toolarge;

void show_options(char *progname);

void scan_options(int n,char **in);

char find_nearest(long n,unsigned int dim,double eps);

int main(int argc,char **argv);
};

#endif
