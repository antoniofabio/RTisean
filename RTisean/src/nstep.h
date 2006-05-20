#ifndef _NS_NSTEP
#define _NS_NSTEP


struct ns_nstep{


char onscreen,epsset,*outfile;
char *infile;
unsigned int nmax;
unsigned int verbosity;
long **box,*list,*found;
double **series,**cast;
double *interval,*min,epsilon;

unsigned int embed,dim,dim1,DELAY;
char *column,dimset,do_zeroth;
int MINN;
unsigned long LENGTH,FLENGTH,exclude;
double EPS0,EPSF;

double **mat,**imat,*vec,*localav,*foreav;

void show_options(char *progname);

void scan_options(int n,char **in);

void put_in_boxes(void);

unsigned int hfind_neighbors(void);

void multiply_matrix(double **mat,double *vec);

void make_fit(int number,double *newcast);

void make_zeroth(int number,double *newcast);

int main(int argc,char **argv);
};


#endif


