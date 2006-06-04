low121 <- function(series,l,x=0,c=1,i=1){
	args <- list(routinename="low121", input=series, x=x,c=c,i=i)
	if(!missing(l))
		args <- c(args, l=l)
	args <- c(args, suffixes=paste(".",i,sep=""))
	out <- do.call(callTISEAN, args)
	out <- out[[1]]
	out <- as.matrix(out)
	return(out)
}
