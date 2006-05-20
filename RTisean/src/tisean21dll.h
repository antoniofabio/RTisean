#include <R.h>
#include <Rdefines.h>


// TISEAN CPP-WRAPPERS (DECLARATIONS)

extern "C"
{

//interface
void call_TISEAN(double* d, int* len, char** args, char** routine, int* p, int* q, int* r);
SEXP read_TISEAN_output(SEXP p, SEXP q, SEXP r);


void ar_model( int *retu, int *retr, char **pstr );
void av_d2( int *retu, int *retr, char **pstr );
void boxcount( int *retu, int *retr, char **pstr ); 
void corr( int *retu, int *retr, char **pstr ); 
void d2( int *retu, int *retr, char **pstr ); 
void extrema( int *retu, int *retr, char **pstr ); 
void false_nearest( int *retu, int *retr, char **pstr );
void fsle( int *retu, int *retr, char **pstr ); 
void ghkss( int *retu, int *retr, char **pstr ); 
void histogram( int *retu, int *retr, char **pstr ); 
void ll_ar( int *retu, int *retr, char **pstr ); 
void low121( int *retu, int *retr, char **pstr );
void lyap_k( int *retu, int *retr, char **pstr ); 
void lyap_r( int *retu, int *retr, char **pstr ); 
void lyap_spec( int *retu, int *retr, char **pstr ); 
void makenoise( int *retu, int *retr, char **pstr ); 
void mem_spec( int *retu, int *retr, char **pstr ); 
void mutual( int *retu, int *retr, char **pstr ); 
void nrlazy( int *retu, int *retr, char **pstr ); 
void nstat_z( int *retu, int *retr, char **pstr ); 
void nstep( int *retu, int *retr, char **pstr ); 
void onestep( int *retu, int *retr, char **pstr );
void poincare( int *retu, int *retr, char **pstr ); 
void polyback( int *retu, int *retr, char **pstr );
void polynom( int *retu, int *retr, char **pstr );
void polynomp( int *retu, int *retr, char **pstr ); 
void polypar( int *retu, int *retr, char **pstr ); 
void rbf( int *retu, int *retr, char **pstr );
void recurr( int *retu, int *retr, char **pstr );
void resample( int *retu, int *retr, char **pstr );
void rescale( int *retu, int *retr, char **pstr );
void sav_gol( int *retu, int *retr, char **pstr );
void svd( int *retu, int *retr, char **pstr );
void xcor( int *retu, int *retr, char **pstr ); 
void xzero( int *retu, int *retr, char **pstr );
void zeroth( int *retu, int *retr, char **pstr );
}

