#ifndef _NS_POLYBACK
#define _NS_POLYBACK

struct ns_polyback{


char *outfile,stdo;
char *parin,*infile;
unsigned long length,insample,exclude;
unsigned int plength;
unsigned int column,dim,delay,down_to,step;
unsigned int **order;
unsigned int verbosity;
double *series,*param;

void show_options(char *progname);

void scan_options(int n,char **in);

double polynom(unsigned long act,unsigned int which);

void make_fit(void);

double forecast_error(unsigned long i0,unsigned long i1);

int main(int argc,char **argv);
};

#endif
