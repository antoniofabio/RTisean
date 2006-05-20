#ifndef _NS_MEM_SPEC
#define _NS_MEM_SPEC


struct ns_mem_spec{



unsigned long poles,out;
unsigned long length,exclude;
unsigned int column;
unsigned int verbosity;
char *outfile,stdo;
char *infile;
double *series;

void show_options(char *progname);

void scan_options(int argc,char **argv);

double getcoefs(double *coef);

double powcoef(double dt,double *coef);

int main(int argc,char **argv);

};

#endif


