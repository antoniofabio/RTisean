/*Author: Rainer Hegger Last modified: Jul 9, 1999 */
void make_multi_box(double **ser,long **box,long *list,unsigned long l,
	      unsigned int bs,unsigned int dim,unsigned int emb,
	      unsigned int del,double eps)
{
  int i,x,y;
  int ib=bs-1;

  for (x=0;x<bs;x++)
    for (y=0;y<bs;y++)
      box[x][y] = -1;
  
  for (i=(emb-1)*del;i<l;i++) {
    x=(int)(ser[0][i]/eps)&ib;
    y=(int)(ser[dim-1][i]/eps)&ib;
    list[i]=box[x][y];
    box[x][y]=i;
  }
}

