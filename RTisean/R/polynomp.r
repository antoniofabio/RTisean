#Needs special calling scheme: accepts multiple files as input options
polynomp <- function(series,l,x=0,c=1,m=2,d=1,n,L=1000,p){
	args <- list(routinename="polynomp", input=series, x=x, c=c, m=m, d=d, L=L)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(n))
		args <- c(args, n=n)
	if(missing(p))
		stop("you must pass a 'p' matrix of coefficients")
	args <- c(args, list(parobjects=list(p=p)))
	out <- as.list(do.call(callTISEAN, args))
	vect <- substring(out[[1]],2)[-1]
	ans <- list()
	ans$coef <- as.matrix( TISEANblock(vect) )
	ans$pred <- as.matrix(out[[2]])
	return(ans)
}
