#ifndef _NS_EXTREMA
#define _NS_EXTREMA

struct ns_extrema{

unsigned long length,exclude;
char *column;
unsigned int verbosity;
unsigned int dim;
unsigned int which;
double mintime;
char dimset;
char maxima;
char stdo;
char *outfile;
char *infile;

void show_options(char *progname);

void scan_options(int n,char **in);

int main(int argc,char **argv);
};

#endif

