#ifndef _AR_MODEL
#define _AR_MODEL

#include <stdio.h>

struct ns_ar_model{

unsigned long length,exclude;
unsigned int dim,poles,ilength;
unsigned int verbosity;
char *outfile,*column,stdo,dimset,run_model;
char *infile;
double **series;

void show_options(char *progname);

void scan_options(int argc,char **argv);

void set_averages_to_zero(void);

double** build_matrix(double **mat);

void build_vector(double *vec,long comp);

double* multiply_matrix_vector(double **mat,double *vec);

double* make_residuals(double **diff,double **coeff);

void iterate_model(double **coeff,double *sigma,FILE *file);

int main(int argc,char **argv);

};

#endif
