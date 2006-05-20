#ifndef _NS_MAKENOISE
#define _NS_MAKENOISE

struct ns_makenoise{

char *outfile,cgaussian,stout,justcreate;
char *infile;
char absolute,dimset;
unsigned long length,exclude,iseed;
unsigned int dim;
char *column;
unsigned int verbosity;
double **array,noiselevel;

void show_options(char *progname);

void scan_options(int n,char** in);

void equidistri(double sigmax,unsigned int which);

void gauss(double sigmax,unsigned int which);

int main(int argc,char** argv);
};

#endif


