lazy <- function(series,m,r,v,i=1,l,x=0,c=1){
	args <- list(routinename="lazy", input=series, i=i, x=x, c=c)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(r))
		args <- c(args, r=r)
	if(!missing(v))
		args <- c(args, v=v)
	if(!missing(l))
		args <- c(args, l=l)
	out <- as.matrix(do.call(callTISEAN, args))
	out <- out[,1]
	return(out)
}
