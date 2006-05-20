logistic=function(iter=1000, r=4,x=0.2,trans=0) {
out=NULL
out[1]=x

  for (i in 2:(iter+trans))
  out[i]=r*out[i-1]*(1-out[i-1])

out=out[(trans+1):(trans+iter)]
return(out)
}