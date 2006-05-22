logistic=function(iter=1000, r=4,x=0.2,t=0,nsd=0) {
out=NULL
out[1]=x+rnorm(1,sd=nsd)

  for (i in 2:(iter+t))
  out[i]=r*out[i-1]*(1-out[i-1])+rnorm(1,sd=nsd)

out=out[(t+1):(t+iter)]
return(out)
}
