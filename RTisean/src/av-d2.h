#ifndef _AV_D2
#define _AV_D2

struct ns_av_d2{
unsigned int maxdim,mindim;
unsigned int verbosity;
int aver;
char rescaled;
char stout;
char *outfile;
char *infile;

void show_options(char *progname);

void scan_options(int n,char **in);

int main(int argc,char **argv);
};

#endif



