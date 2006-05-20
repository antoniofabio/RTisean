#ifndef _NS_LOW121
#define _NS_LOW121

struct ns_low121{

unsigned long length,exclude;
unsigned int column,iterations;
unsigned int verbosity;
char *outfile,stdo;
char *infile;

double *series,*ynew;

void show_options(char *progname);

void scan_options(int n,char **in);

int main(int argc,char **argv);
};

#endif


