RT_predict <- function(series,d,m,r,v,s=1,l,x=0,c=1){
	args <- list(routinename="predict", input=series, s=s,x=x,c=c)
	if(!missing(d))
		args <- c(args, d=d)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(v))
		args <- c(args, v=v)
	if(!missing(l))
		args <- c(args, l=l)
	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
