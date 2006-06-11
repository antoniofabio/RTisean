wiener1 <- function(series,f,w,l,x=0,c=1){
	args <- list(routinename="wiener1", input=series, x=x, c=c)
	if(!missing(f))
		args <- c(args, f=f)
	if(!missing(w))
		args <- c(args, w=w)
	if(!missing(l))
		args <- c(args, l=l)
	out <- as.matrix(do.call(callTISEAN, args))
	colnames(out) <- c("period","value")
	rownames(out) <- NULL
	return(out)
}
