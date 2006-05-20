#ifndef _NS_RBF
#define _NS_RBF


struct ns_rbf{

char *outfile,stdo,MAKECAST;
char *infile;
char setdrift;
int DIM,DELAY,CENTER,STEP;
unsigned int COLUMN;
unsigned int verbosity;
long CLENGTH;
unsigned long LENGTH,INSAMPLE,exclude;

double *series,*coefs;
double varianz,interval,min;
double **center;

void show_options(char *progname);

void scan_options(int n,char **in);

double avdistance(void);

double rbf(double *act,double *cen);

void drift(void);

void make_fit(void);

double forecast_error(unsigned long i0,unsigned long i1);

void make_cast(FILE *out);

int main(int argc,char **argv);
};

#endif


