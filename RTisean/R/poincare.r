poincare <- function(series,l,x=0,c=1,m=2,d=1,q,C=0,a){
	args <- list(routinename="poincare", input=series, x=x, c=c, m=m, d=d, C=C)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(q))
		args <- c(args, q=q)
	if(!missing(a))
		args <- c(args, a=a)

	out <- as.matrix(do.call(callTISEAN, args))
	colnames(out) <- c(paste("dim",1:(NCOL(out)-1),sep=""),"time")
	return(out)
}
