#ifndef _NS_POINCARE
#define _NS_POINCARE

struct ns_poincare{


char *outfile,dimset,compset,whereset,stdo;
char *infile;
unsigned long length,count,exclude;
int dim,comp,delay,dir;
unsigned int column;
unsigned int verbosity;
double *series,min,max,average,where;

void show_options(char *progname);


void scan_options(int n,char** in);

void poincare(void);

int main(int argc,char** argv);
};

#endif

