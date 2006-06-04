av_d2 <- function(lst,m=1,M,a=1,E=FALSE){
	if(!inherits(lst,"list"))
		stop("wrong input")
	class(lst) <- c("TISEANmatrixlist","list") #needs special serialization method
	args <- list(routinename="av-d2", input=lst, m=m, a=a)
	if(!missing(M))
		args <- c(args, list(M=M))
	if(E)
		args <- c(args, list("E"))
	out <- do.call(callTISEAN, args)
	out <- as.matrixList(as.list(out))
	nl <- floor(length(out)/2)
	out <- colnamesout(out[(0:(nl-1))*2+1],0,c("epsilon","smoothed d2"))
	return(out)
}
