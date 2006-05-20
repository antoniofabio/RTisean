#ifndef _NS_RESCALE
#define _NS_RESCALE


struct ns_rescale{

unsigned long length,exclude;
unsigned int dim;
unsigned int verbosity;
char *column;
char *outfile,stdo,set_av,set_var,dimset;
char *infile;
double **series;
double xmin,xmax;

void show_options(char *progname);

void scan_options(int n,char **in);

int main(int argc,char **argv);
};

#endif

