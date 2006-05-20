#ifndef _NS_POLYPAR
#define _NS_POLYPAR


struct ns_polypar{


char *outfile;
unsigned int dim,order;
unsigned int verbosity;
FILE *file;



void show_options(char *progname);

void scan_options(int n,char **in);

void make_parameter(unsigned int *par,unsigned int d,unsigned int sum);

int main(int argc,char **argv);
};

#endif

