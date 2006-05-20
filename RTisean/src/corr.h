#ifndef _NS_CORR
#define _NS_CORR

struct ns_corr{


char *format,*outfile,stout,normalize;
unsigned int column;
unsigned int verbosity;
unsigned long tau,length,exclude;
double *array;
double av,var;
char *infile;

void show_options(char *progname);

void scan_options(int argc,char **argv);

double corr(long i);

int main(int argc,char** argv);
};

#endif

