#ifndef _NS_SAV_GOL
#define _NS_SAV_GOL


struct ns_sav_gol{

unsigned long length,exclude;
unsigned int dim;
char dimset;
char *columns;
unsigned int nf,nb,power,deriv;
char *infile,*outfile,stdo;
unsigned int verbosity;

double **series;

void show_options(char *progname);

void scan_options(int n,char **argv);

double** make_coeff(void);

double make_norm(void);

int main(int argc,char **argv);
};

#endif

