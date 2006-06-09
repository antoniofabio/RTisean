c2d <- function(lst,a=1){
	if(!inherits(lst,"list"))
		stop("wrong input")
	class(lst) <- c("TISEANmatrixlist","list") #needs special serialization method

	out <- as.list(callTISEAN("c2d",input=lst,a=a))
	out <- as.matrixList(out)
	out <- colnamesout(out,1,c("epsilon","local.slope"))
	out <- .cleanList(out)
	return(out)
}
