#ifndef _NS_LYAP_R
#define _NS_LYAP_R

#define NMAX 256


struct ns_lyap_r{

char *outfile;
char *infile;
char epsset;
double *series,*lyap;
long box[NMAX][NMAX],*list;
unsigned int dim,delay,steps,mindist;
unsigned int column;
unsigned int verbosity;
unsigned int nmax;
unsigned long length,exclude;
long *found;
double eps0,eps,epsinv;

void show_options(char *progname);

void scan_options(int n,char **argv);
      
void put_in_boxes(void);

char make_iterate(long act);

int main(int argc,char **argv);
};

#endif

