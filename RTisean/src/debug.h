#ifndef __DEBUG_H__
#define __DEBUG_H__

#ifdef RT_DEBUG
#define RT_TRACE Rprintf
#else
#define RT_TRACE
#endif

#endif
