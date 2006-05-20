#ifndef _NS_FSLE
#define _NS_FSLE


#define NMAX 256


struct ns_fsle{


char *outfile;
char *infile;
char epsset,stdo;
double *series;
long box[NMAX][NMAX],*list;
unsigned int dim,delay,mindist;
unsigned int column;
unsigned int verbosity;
unsigned int nmax;
unsigned long length,exclude;
double eps0,eps,epsinv,epsmax,epsfactor;
int howmany;


void show_options(char *progname);

void scan_options(int n,char **argv);
      
void put_in_boxes(void);

char make_iterate(long act);

int main(int argc,char **argv);

};

#endif
