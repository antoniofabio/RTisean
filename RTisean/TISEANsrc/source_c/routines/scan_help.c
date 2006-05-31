/*Author: Rainer Hegger Last modified: March 29th, 1998 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int scan_help(int n,char **in)
{
  int i;
  
  for (i=1;i<n;i++)
    if ((in[i][0] == '-') && (in[i][1] == 'h'))
      return 1;
  return 0;
}
