#ifndef _NS_MUTUAL
#define _NS_MUTUAL


struct ns_mutual{


char *file_out,stout;
char *infile;
unsigned long length,exclude;
unsigned int column;
unsigned int verbosity;
long partitions,corrlength;
long *array,*h1,*h11,**h2;

void show_options(char *progname);

void scan_options(int n,char** in);

double make_cond_entropy(long t);

int main(int argc,char** argv);

};

#endif


