/*Author: Thomas Schreiber Last modified: 2.Sep, 1999 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void what_i_do(char *name,char *text)
{
  fprintf(stderr, "\nTISEAN 2.1 (C) R. Hegger, H. Kantz, T. Schreiber\n\n"
     "%s: %s\n\n",name,text);
}

