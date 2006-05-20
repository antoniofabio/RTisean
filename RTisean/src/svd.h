#ifndef _NS_SVD
#define _NS_SVD


struct ns_svd{


unsigned long LENGTH,exclude;
unsigned int DIM,LDIM,COLUMN,DELAY;
unsigned int verbosity;
char *outfile,stout,make_base,dim_set;
char *infile;
double *series,av;

void show_options(char *progname);

void scan_options(int n,char **in);

void ordne(double *lyap,int *ord);

void make_pca(void);

int main(int argc,char **argv);
};

#endif

