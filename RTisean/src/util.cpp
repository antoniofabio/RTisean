#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util.h"

static int find_non_space(char *str, int start);
static int find_space(char *str, int start);

//----------------------------------------------------------------------------------------------
// returns 0 if OK. 1 - otherwise.
//
int findargs(char **nstr, int *argc, char*** argv, char *str, char* arg0)
{
	int i, pos1, pos2, len, c;
	char **v = 0;
	char *p = 0;
		
	len = strlen(str);	
	p = new char[ len + 2];
	if (p == NULL)
		return 1;

	strcpy(p, str);
	p[len + 1] = 0;
	
	c = 1;
	pos1 = -1;
	pos2 = 0;
	do
	{
		pos1 = find_non_space(p, pos2);
		if (pos1 == -1)
			break;

		c += 1;
		pos2 = find_space(p, pos1);
	}
	while (true);
	
	v = new char*[c];
	if (v == NULL)
	{
		delete p;
		return 1;
	}

	v[0] = arg0;
	pos1 = -1;
	pos2 = 0;
	i = 0;
	do
	{
		pos1 = find_non_space(p, pos2);
		if (pos1 == -1)
			break;
		
		++i;
		v[i] = p + pos1;
		pos2 = find_space(p, pos1);
		p[pos2] = 0;
		pos2 += 1;
	}
	while (true);

	*nstr = p;
	*argc = c;
	*argv = v;

	return 0;
}

//----------------------------------------------------------------------------------------------

static int find_non_space(char *str, int start)
{
	while(*(str + start) != 0)
	{
		if (!isspace(str[start]))
		{
			return start;
		}
		
		++start;
	}

	return -1;
}

//----------------------------------------------------------------------------------------------

static int find_space(char *str, int start)
{
	while(*(str + start) != 0)
	{
		if (isspace(str[start]))
		{
			return start;
		}
		
		++start;
	}

	return start;
}



