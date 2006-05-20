#ifndef NS_LYAP_K
#define NS_LYAP_K

#define BOX 128


struct ns_lyap_k{

unsigned int ibox;

unsigned long length;
unsigned long exclude;
unsigned long reference;
unsigned int maxdim;
unsigned int mindim;
unsigned int delay;
unsigned int column;
unsigned int epscount;
unsigned int maxiter;
unsigned int window;
unsigned int verbosity;
double epsmin,epsmax;
char eps0set,eps1set;
char *outfile;
char *infile;

double *series,**lyap;
long box[BOX][BOX],*liste,**lfound,*found,**count;
double max,min;

void show_options(char *progname);

void scan_options(int n,char **str);

void put_in_boxes(double eps);

void lfind_neighbors(long act,double eps);

void iterate_points(long act);

int main(int argc,char **argv);
};

#endif


