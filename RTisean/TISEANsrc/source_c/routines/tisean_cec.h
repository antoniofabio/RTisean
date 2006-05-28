/*Author: Rainer Hegger Last modified: May 26, 2000*/

/* These definitions give the exit codes for the C part of the Tisean package.
   Typically the name is build up of, first, the name of the routine creating
   the exception, secondly, sort of an description of the exception.
   */

#ifndef _TISEAN_CEC_H
#define _TISEAN_CEC_H

/* These are the codes for the routines subtree */
#define RESCALE_DATA_ZERO_INTERVAL 11
#define CHECK_ALLOC_NOT_ENOUGH_MEMORY 12
#define CHECK_OPTION_NOT_UNSIGNED 13
#define CHECK_OPTION_NOT_INTEGER 14
#define CHECK_OPTION_NOT_FLOAT 15
#define CHECK_OPTION_NOT_TWO 16
#define CHECK_OPTION_C_NO_VALUE 17
#define TEST_OUTFILE_NO_WRITE_ACCESS 18
#define SOLVELE_SINGULAR_MATRIX 19
#define GET_SERIES_NO_LINES 20
#define GET_MULTI_SERIES_WRONG_TYPE_OF_C 21
#define GET_MULTI_SERIES_NO_LINES 22
#define VARIANCE_VAR_EQ_ZERO 23
#define EIG2_TOO_MANY_ITERATIONS 24

/* These are the codes for the main routines */
#define LYAP_SPEC_NOT_ENOUGH_NEIGHBORS 50
#define LYAP_SPEC_DATA_TOO_SHORT 51
#define AR_MODEL_TOO_MANY_POLES 52
#define EXTREMA_STRANGE_COMPONENT 53
#define FALSE_NEAREST_NOT_ENOUGH_POINTS 54
#define FSLE__TOO_LARGE_MINEPS 55
#define GHKSS__TOO_MANY_NEIGHBORS 56
#define NSTAT_Z__INVALID_STRING_FOR_OPTION 57
#define NSTAT_Z__NOT_UNSIGNED_FOR_OPTION 58
#define NSTAT_Z__TOO_LARGE_FOR_OPTION 59
#define NSTAT_Z__OPTION_NOT_SET 60
#define NSTAT_Z__TOO_MANY_PIECES 61
#define NSTEP__ESCAPE_REGION 62
#define POINCARE__WRONG_COMPONENT 63
#define POINCARE__OUTSIDE_REGION 64
#define POLYBACK__WRONG_PARAMETER_FILE 65
#define POLYNOMP__WRONG_PARAMETER_FILE 66
#define RESCALE__WRONG_INTERVAL 67
#define SAV_GOL__UNDERDETERMINED 68
#define SAV_GOL__TOO_LARGE_DERIVATIVE 69
#define MAKENOISE__FLAGS_REQUIRED 70
#define ZEROTH__STEP_TOO_LARGE 71
#define LYAP_K__MAXITER_TOO_LARGE 72

/* Global stuff */
#define VECTOR_TOO_LARGE_FOR_LENGTH 100

#endif
