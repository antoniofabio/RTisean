project <- function(series,m,q,r,k,i=1,l,x=0,c=1){

	args <- list(routinename="project", input=series, i=i, x=x, c=c)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(q))
		args <- c(args, q=q)
	if(!missing(r))
		args <- c(args, r=r)
	if(!missing(k))
		args <- c(args, k=k)
	if(!missing(l))
		args <- c(args, l=l)

	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
