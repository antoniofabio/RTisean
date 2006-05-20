#ifndef _NS_NSTAT_Z
#define _NS_NSTAT_Z


struct ns_nstat_z{


unsigned int nmax;
long **box,*list;
unsigned long *found;
double *series,*series1,*series2;
double interval,min,epsilon;

char epsset,causalset;
char *infile;
char *outfile,stdo,centerset;
char *firstwindow,*secondwindow,**window;
unsigned int COLUMN,pieces;
unsigned int verbosity;
int DIM,DELAY,MINN,STEP;
int firstoffset,secondoffset;
double EPS0,EPSF;
unsigned long LENGTH,exclude,center,causal;

void show_options(char *progname);

void parse_minus(char *str,char *array,char *wopt);
void parse_comma(char *str,char *array,char *wopt);

void parse_out(char *str,char *array,char *which);

void parse_offset(char *str,int *iwhich,char *array,char *which);
      
void scan_options(int n,char **in);

double make_fit(long act,unsigned long number);

int main(int argc,char **argv);

};

#endif



