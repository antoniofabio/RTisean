#ifndef __DEBUG_H__
#define __DEBUG_H__
#include <R.h>

#ifdef RT_DEBUG
#define RT_TRACE(__VA_ARGS__) do { \
	Rprintf("In %s:%d\t",__FILE__, __LINE__); \
	Rprintf(__VA_ARGS__); \
} while(0)
#else
#define RT_TRACE(__VA_ARGS__) 
#endif

#endif
