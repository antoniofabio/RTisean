#ifndef _NS_HISTOGRAM
#define _NS_HISTOGRAM


struct ns_histogram{

unsigned long length;
unsigned long base;
unsigned long exclude;
unsigned int column;
unsigned int verbosity;
double size;
char my_stdout,gotsize;
char *outfile;
char *infile;

double *series;
double average,var;
double min,max;
unsigned int bsize,bsize1;
long *box,*list;

void show_options(char* progname);

void scan_options(int n,char **str);

void put_in_boxes(void);

unsigned long neighbors(double x);

int main(int argc,char **argv);
};

#endif


