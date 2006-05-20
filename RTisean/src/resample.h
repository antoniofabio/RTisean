#ifndef _NS_RESAMPLE
#define _NS_RESAMPLE


struct ns_resample{

unsigned long length,exclude;
unsigned int column,order;
unsigned int verbosity;
char *outfile,stdo;
char *infile;
double *series,sampletime;

void show_options(char *progname);

void scan_options(int argc,char **argv);

int main(int argc,char **argv);
};

#endif



