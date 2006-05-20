/*Author: Rainer Hegger Last modified: Aug 19, 1999 */

/* modified by Alexei Grigoriev, 27.4.2006 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "tisean_cec.h"

#include <setjmp.h>
extern jmp_buf my_exit_point;

extern void check_alloc(void*);
/* possible types are
   'd'  (long) integer
   'u'  unsigned (long)
   '2'  two unsigned (long) numbers separated by a comma
   'f'  float
   's'  string
   'o'  optional string (must only begin with a minus if there is no space)
   'n'  no parameter
   */

void check_unsigned(char *tocheck,int which)
{
  int i,n;
  char ok=1;

  n=strlen(tocheck);
  for (i=0;i<n;i++)
    if (!isdigit((unsigned int)tocheck[i]))
      ok=0;

  if (!ok) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be an "
	    "unsigned integer\n",which);
    exit(CHECK_OPTION_NOT_UNSIGNED);
  }
}

void check_integer(char *tocheck,int which)
{
  int i,n;
  char ok=1;

  n=strlen(tocheck);
  ok=(tocheck[0] == '-') || isdigit((unsigned int)tocheck[0]);
  if (ok)
    for (i=1;i<n;i++)
      if (!isdigit((unsigned int)tocheck[i]))
	ok=0;
  
  if (!ok) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be an "
	    "integer\n",which);
    exit(CHECK_OPTION_NOT_INTEGER);
  }
}

void check_float(char *tocheck,int which)
{
  double dummy;
  int found;
  char *rest;
  
  check_alloc(rest=(char*)calloc(strlen(tocheck)+1,(size_t)1));
  found=sscanf(tocheck,"%lf%s",&dummy,rest);
  if (found != 1) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be a "
	    "float\n",which);
    exit(CHECK_OPTION_NOT_FLOAT);
  }
  free(rest);
}

void check_two(char *tocheck,int which)
{
  int i,j;
  unsigned int len;

  len=(unsigned int)strlen(tocheck);
  for (i=0;i<len;i++)
    if (tocheck[i] == ',')
      break;
  if (i >= (len-1)) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be"
	    " unsigned,unsigned\n",which);
    exit(CHECK_OPTION_NOT_TWO);
  }
  for (j=0;j<i;j++)
    if (!isdigit((unsigned int)tocheck[j])) {
      longjmp(my_exit_point,4);
      fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be"
	      " unsigned,unsigned\n",which);
      exit(CHECK_OPTION_NOT_TWO);
    }
  for (j=i+1;j<len;j++)
    if (!isdigit((unsigned int)tocheck[j])) {
      longjmp(my_exit_point,4);
      fprintf(stderr,"Wrong type of parameter for flag -%c. Has to be"
	      " unsigned,unsigned\n",which);
      exit(CHECK_OPTION_NOT_TWO);
    }
}

char check_optional(char *tocheck,int which)
{
  if (tocheck[0] == '-') {
    fprintf(stderr,"If you want to give the -%c flag a parameter starting"
	    " with a - don't put a space. Ignoring it.\n",which);
    return 0;
  }
  return 1;
}

char* check_option(char **in,int n,int which,int type)
{
  char test,*ret=NULL,wasfound=0,ok=1;
  int i;
  
  for (i=1;i<n;i++) {
    if (in[i] != NULL) {
      test= (in[i][0] == '-') && (in[i][1] == which);
      if (test) {
	wasfound=1;
	if (type != 'n') {
	  if (strlen(in[i]) > 2) {
	    switch(type) {
	    case 'u': check_unsigned(in[i]+2,which);break;
	    case 'd': check_integer(in[i]+2,which);break;
	    case 'f': check_float(in[i]+2,which);break;
	    case '2': check_two(in[i]+2,which);break;
	    }
	    if (ret != NULL)
	      free(ret);
	    check_alloc(ret=(char*)calloc(strlen(in[i]+2)+1,(size_t)1));
	    strcpy(ret,in[i]+2);
	    in[i]=NULL;
	  }
	  else {
	    in[i]=NULL;
	    i++;
	    if (i < n) {
	      if (in[i] != NULL) {
		switch(type) {
		case 'u': check_unsigned(in[i],which);break;
		case 'd': check_integer(in[i],which);break;
		case 'f': check_float(in[i],which);break;
		case '2': check_two(in[i],which);break;
		case 'o': ok=check_optional(in[i],which);break;
		}
		if (ok) {
		  if (ret != NULL)
		    free(ret);
		  check_alloc(ret=(char*)calloc(strlen(in[i])+1,(size_t)1));
		  strcpy(ret,in[i]);
		  in[i]=NULL;
		}
		else {
		  i--;
		  if (ret != NULL)
		    free(ret);
		  ret=NULL;
		}
	      }
	    }
	    else {
	      if (ret != NULL) {
		free(ret);
		ret=NULL;
	      }
	    }
	  }
	}
	else {
	  in[i]=NULL;
	}
      }
    }
  }
  
  if (((type == 'o') || (type == 'n')) && (ret == NULL) && wasfound)
    return "";

  if (wasfound && (ret == NULL)) {
    longjmp(my_exit_point,4);
    fprintf(stderr,"The option -%c needs some value. Exiting!\n",which);
    exit(CHECK_OPTION_C_NO_VALUE);
  }
  return ret;
}

