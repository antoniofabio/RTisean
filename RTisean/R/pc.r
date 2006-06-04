pc <- function(series,m,d=1,q=2,l,x=0,c=1){
	args <- list(routinename="pc", input=series, d=d, q=q, x=x, c=c)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(l))
		args <- c(args, l=l)
	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
