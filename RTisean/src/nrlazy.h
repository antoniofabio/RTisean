#ifndef _NS_NRLAZY
#define _NS_NRLAZY

struct ns_nrlazy{


unsigned long length,exclude;
unsigned int column,dim,delay,iterations;
unsigned int verbosity;
double eps,epsvar;

char *outfile,epsset,stdo,epsvarset;
char *infile;
double *series,*corr,interval,min;
long **box,*list,*nf;

void show_options(char *progname);

void scan_options(int n,char **in);

unsigned int correct(unsigned long n);

int main(int argc,char **argv);

};

#endif


