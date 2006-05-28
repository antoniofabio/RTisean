/*Author: Rainer Hegger Last modified: Sep 4, 1999 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tsa.h"

char* myfgets(char *str,int *size,FILE *fin,unsigned int verbosity)
{
  char *ret;
  char *hstr=NULL;
  char last;

  ret=fgets(str,*size,fin);
  if (ret == NULL)
    return NULL;

  last=str[strlen(str)-1];

  while (last != '\n') {
    *size += INPUT_SIZE;
    check_alloc(hstr=(char*)calloc((size_t)INPUT_SIZE,(size_t)1));
    check_alloc(str=realloc(str,(size_t)*size));
    ret=fgets(hstr,INPUT_SIZE,fin);
    strcat(str,hstr);
    if (verbosity&VER_INPUT)
      fprintf(stderr,"Line in file too long. Increasing input size\n");
    last=str[strlen(str)-1];
    free(hstr);
  }

  if (ret == NULL)
    return NULL;
  else
    return str;
}
