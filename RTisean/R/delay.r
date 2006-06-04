RT_delay <- function(series,d=1,m=2,l,x=0,c=1){
	args <- list(routinename = "delay", input=series, d=d, m=m, x=x, c=c)
	if(!missing(l))
		args <- c(args, l)
	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
