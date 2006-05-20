#ifndef _NS_POLYNOMP
#define _NS_POLYNOMP


struct ns_polynomp{


char *outfile,stdo;
char *parin,*infile;
unsigned long length,insample,exclude;
unsigned int plength;
unsigned long step;
unsigned int column,dim,delay,down_to;
unsigned int **order;
unsigned int verbosity;
double *series,*param;

void show_options(char *progname);

void scan_options(int n,char **in);

double polynom(unsigned long act,unsigned int which);

void make_fit(void);

double forecast_error(unsigned long i0,unsigned long i1);

void make_cast(FILE *fcast);

int main(int argc,char **argv);
};

#endif

