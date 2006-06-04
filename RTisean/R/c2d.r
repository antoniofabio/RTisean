c2d <- function(lst,a=1){
	if(!inherits(lst,"list"))
		stop("wrong input")
	class(lst) <- c("TISEANmatrixlist","list") #needs special serialization method

	out <- as.list(callTISEAN("c2d",input=lst,a=a))
	out <- as.matrixList(out)
	out <- colnamesout(out,1,c("epsilon","local.slope"))
	nl <- floor(length(out)/2)
	nms <- unlist(out[(1:nl-1)*2+1])
	out <- out[(1:length(nms))*2]
	names(out) <- nms
	return(out)
}
