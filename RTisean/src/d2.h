#ifndef _NS_D2
#define _NS_D2

#define NMAX 256


struct ns_d2{

double **series;
long *scr;
char dimset,rescale_set,eps_min_set,eps_max_set;
char *FOUT;
double epsfactor,epsinv,lneps,lnfac;
double EPSMAX,EPSMIN;
double min,interval;
int imax,howoften1,imin;
long box[NMAX][NMAX],*list,boxc1[NMAX],*listc1;
unsigned long nmax;
double **found,*norm;
unsigned long MINDIST,MAXFOUND;
unsigned long length,exclude;
unsigned int DIM,EMBED,HOWOFTEN,DELAY;
unsigned int verbosity;
char *column;
char *infile;

void show_options(char *progname);

void scan_options(int n,char **argv);
      
void scramble(void);

void make_c2_dim(int n);

void make_c2_1(int n);

int main(int argc,char **argv);
};

#endif

