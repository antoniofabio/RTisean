#include <stdlib.h>
#include "tisean21dll.h"
#include "util.h"


#include "ar-model.h"
#include "av-d2.h"
#include "boxcount.h"
#include "corr.h"
#include "d2.h"
#include "extrema.h"
#include "false_nearest.h"
#include "fsle.h"
#include "ghkss.h"
#include "histogram.h"
#include "ll-ar.h"
#include "low121.h"
#include "lyap_k.h"
#include "lyap_r.h"
#include "lyap_spec.h"
#include "makenoise.h"
#include "mem_spec.h"
#include "mutual.h"
#include "nrlazy.h"
#include "nstat_z.h"
#include "nstep.h"
#include "onestep.h"
#include "poincare.h"
#include "polyback.h"
#include "polynom.h"
#include "polynomp.h"
#include "polypar.h"
#include "rbf.h"
#include "recurr.h"
#include "resample.h"
#include "rescale.h"
#include "sav_gol.h"
#include "svd.h"
#include "xcor.h"
#include "xzero.h"
#include "zeroth.h"



extern "C"{

void ar_model ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ 
		*retr=0; 
		return; 
	}
	ns_ar_model* obj = new ns_ar_model;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void av_d2 ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ 
		*retr=0; 
		return; 
	} 
	ns_av_d2* obj = new ns_av_d2;
	*retr = (*obj).main(argc, argv);  
	free(argv); 
	free(p); 
	delete obj;
} 
void boxcount ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_boxcount* obj = new ns_boxcount;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void corr ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_corr* obj = new ns_corr;
	*retr = (*obj).main(argc, argv);	 
	free(argv); 
	free(p); 
	delete obj;
} 
void d2 ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_d2* obj = new ns_d2;
	*retr = (*obj).main(argc, argv);	
	free(argv); 
	free(p); 
	delete obj;
} 
void extrema ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_extrema* obj = new ns_extrema;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void false_nearest ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_false_nearest* obj = new ns_false_nearest;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void fsle ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_fsle* obj = new ns_fsle;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void ghkss ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_ghkss* obj = new ns_ghkss;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void histogram ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_histogram* obj = new ns_histogram;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void ll_ar ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_ll_ar* obj = new ns_ll_ar;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void low121 ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_low121* obj = new ns_low121;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void lyap_k ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_lyap_k* obj = new ns_lyap_k;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void lyap_r ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_lyap_r* obj = new ns_lyap_r;
	*retr = (*obj).main(argc, argv);  
	free(argv); 
	free(p);
	delete obj; 
} 
void lyap_spec ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; 
	return; } 
	ns_lyap_spec* obj = new ns_lyap_spec;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void makenoise ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_makenoise* obj = new ns_makenoise;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void mem_spec ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_mem_spec* obj = new ns_mem_spec;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p);
	delete obj; 
} 
void mutual ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_mutual* obj = new ns_mutual;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void nrlazy ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_nrlazy* obj = new ns_nrlazy;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p);
	delete obj; 
} 
void nstat_z ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_nstat_z* obj = new ns_nstat_z;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void nstep ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_nstep* obj = new ns_nstep;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void onestep ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_onestep* obj = new ns_onestep;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void poincare ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_poincare* obj = new ns_poincare;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void polyback ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_polyback* obj = new ns_polyback;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void polynom ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_polynom* obj = new ns_polynom;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void polynomp ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_polynomp* obj = new ns_polynomp;
	*retr = (*obj).main(argc, argv);
	free(argv); 
	free(p); 
	delete obj;
} 
void polypar ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_polypar* obj = new ns_polypar;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void rbf ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_rbf* obj = new ns_rbf;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void recurr ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_recurr* obj = new ns_recurr;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void resample ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_resample* obj = new ns_resample;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void rescale ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_resample* obj = new ns_resample;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void sav_gol ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_sav_gol* obj = new ns_sav_gol;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void svd ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_svd* obj = new ns_svd;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void xcor ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	ns_xcor* obj = new ns_xcor;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p);
	delete obj; 
} 
void xzero ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_xzero* obj = new ns_xzero;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 
void zeroth ( int *retu, int* retr, char **pstr ){ 
	char *p; 
	int argc; 
	char** argv; 
	*retu = findargs(&p, &argc, &argv, *pstr, ""); 
	if (*retu){ *retr=0; return; } 
	ns_zeroth* obj = new ns_zeroth;
	*retr = (*obj).main(argc, argv); 
	free(argv); 
	free(p); 
	delete obj;
} 

}





