lyap_r <- function(series,l,x=0,c=1,m=2,d=1,t=0,r,s=50){
	args <- list(routinename = "lyap_r", input=series, x=x, c=c, m=m, d=d, t=t, s=s)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(r))
		args <- c(args, r=r)

	out <- as.matrix(do.call(callTISEAN, args))
	colnames(out) <- c("time","log.stretch")
	return(out)
}
