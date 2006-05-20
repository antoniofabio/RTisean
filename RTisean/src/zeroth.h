#ifndef _NS_ZEROTH
#define _NS_ZEROTH


struct ns_zeroth{


unsigned int nmax;
long **box,*list;
unsigned long *found;
double **series,**diffs;
double interval,min,epsilon;

char epsset,dimset,clengthset,causalset;
char *infile;
char *outfile,stdo;
char *COLUMNS;
unsigned int embed,dim,DELAY,MINN;
unsigned long STEP,causal;
unsigned int verbosity;
double EPS0,EPSF;
unsigned long refstep;
unsigned long LENGTH,exclude,CLENGTH;

void show_options(char *progname);

void scan_options(int n,char **in);

void make_fit(long act,unsigned long number,long istep,double **error);

int main(int argc,char **argv);
};

#endif

