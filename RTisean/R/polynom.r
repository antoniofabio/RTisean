polynom <- function(series,l,x=0,c=1,m=2,d=1,p=2,n,L){
	args <- list(routinename="polynom", input=series, x=x,c=c,m=m,d=d,p=p)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(n))
		args <- c(args, n=n)
	if(!missing(L))
		args <- c(args, L=L)
	out <- as.list(do.call(callTISEAN, args))
	#Output post-processing
	#head#
	ans <- list()
	vect <- substring(out[[1]], 2) #remove starting '#'
	iserr <- grep("insample", vect)
	ans$coeff <- as.matrix( TISEANblock( vect[3:(iserr-1)] ) )
	errstr <- vect[iserr]
	ans$err <- as.numeric( gsub("average insample error= (.*)","\\1",errstr) )
	if(length(outerr <- grep("average out",vect))==1)
		ans$err[2] <- as.numeric( gsub("average out of sample error= (.*)","\\1",vect[outerr]) )
	#body#
	if(length(out)>1)
		ans$pred <- as.matrix(out[[2]])
	return(ans)
}
