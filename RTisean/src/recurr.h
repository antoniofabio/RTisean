#ifndef _NS_RECURR
#define _NS_RECURR


struct ns_recurr{

unsigned long length,exclude;
unsigned int embed,dim,delay;
unsigned int verbosity;
double eps,fraction;
char dimset;
char *columns;
char *outfile,stdo;
char *infile;
char epsset;

double **series;
long **box,*list;

void show_options(char *progname);

void scan_options(int n,char **in);

void lfind_neighbors(void);

int main(int argc,char **argv);
};

#endif
