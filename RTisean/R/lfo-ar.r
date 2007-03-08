lfo.ar <- function(series,l,x=0,c=1,m=c(1,2), d=1,i,r,R,f=1.2,s=1,C){
	args <- list(routinename="lfo-ar", input=series, x=x,c=c,m=m,d=d,f=f,s=s)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(i))
		args <- c(args, i=i)
	if(!missing(r))
		args <- c(args, r=r)
	if(!missing(R))
		args <- c(args, R=R)
	if(!missing(C))
		args <- c(args, C=C)

	out <- as.matrixList(as.list(do.call(callTISEAN, args)))[[2]]
	colnames(out) <- c("neigh.size","rel.error", "fraction", "avg.neigh.", "variance" )
	return(out)
}

ll_ar <- lfo.ar
