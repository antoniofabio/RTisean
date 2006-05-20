#ifndef _BOX_COUNT
#define _BOX_COUNT

typedef struct {
  double *hist;
  void *ptr;
} hliste;

struct ns_boxcount{

unsigned long LENGTH,exclude;
unsigned int maxembed,dimension,DELAY,EPSCOUNT;
unsigned int verbosity;
double Q,EPSMIN,EPSMAX;
char dimset,epsminset,epsmaxset;
char *outfile;
char *column;

int epsi;
unsigned long length;
double EPSFAKTOR;
unsigned int **which_dims;
double *histo;
double **series;

void show_options(char *progname);

void scan_options(int n,char **in);

hliste *make_histo(void);

void next_dim(int wd,int n,unsigned int *first);

void start_box(void);

int main(int argc,char **argv);
};

#endif
