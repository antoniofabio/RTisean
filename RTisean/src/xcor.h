#ifndef _NS_XCOR
#define _NS_XCOR


struct ns_xcor{


char *columns,*outfile,stout;
unsigned long length,exclude;
long tau;
unsigned int verbosity;
double *array1,*array2;
char *infile;

void show_options(char *progname);

void scan_options(int argc,char **argv);

double corr(long i);

int main(int argc,char** argv);
};

#endif
