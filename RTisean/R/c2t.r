c2t <- function(lst){
	if(!inherits(lst,"list"))
		stop("wrong input")
	class(lst) <- c("TISEANmatrixlist","list") #needs special serialization method

	out <- as.list(callTISEAN("c2t",input=lst))
	out <- as.matrixList(out)
	out <- colnamesout(out,1,c("epsilon","log.der."))
	out <- .cleanList(out)
	return(out)
}
