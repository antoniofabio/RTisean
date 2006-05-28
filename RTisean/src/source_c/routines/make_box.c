/*Author: Rainer Hegger Last modified: March 1st, 1998 */
void make_box(double *ser,long **box,long *list,unsigned long l,
		 unsigned int bs,unsigned int dim,unsigned int del,double eps)
{
  int i,x,y;
  int ib=bs-1;

  for (x=0;x<bs;x++)
    for (y=0;y<bs;y++)
      box[x][y] = -1;
  
  for (i=(dim-1)*del;i<l;i++) {
    x=(int)(ser[i-(dim-1)*del]/eps)&ib;
    y=(int)(ser[i]/eps)&ib;
    list[i]=box[x][y];
    box[x][y]=i;
  }
}

