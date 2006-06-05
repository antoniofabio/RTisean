rbf <- function(series,l,x=0,c=1,m=2,d=1,p=10,X=FALSE,s=1,n,L){
	args <- list(routinename="rbf", input=series, x=x, c=c, m=m, d=d, p=p, s=s)
	if(X)
		args <- c(args, list("X"))
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(n))
		args <- c(args, n=n)
	if(!missing(L))
		args <- c(args, L=L)

	out <- as.list(do.call(callTISEAN, args))
	ans <- list()
	#Output post-processing
	#head#
	vect <- out[[1]]
	vect <- substring(vect, 2) #remove starting '#'
	ivar <- grep("variance",vect)
	ans$centers <- as.matrix( TISEANblock( vect[2:(ivar-1)] ) )
	varstr <- vect[ivar]
	ans$variance <- as.numeric( gsub("variance= (.*)","\\1",varstr) )
	iserror <- grep("insample error",vect)
	ans$coefficients <- c( as.matrix( TISEANblock( vect[(ivar+2):(iserror-1)] ) ) )
	errstr <- vect[iserror]
	ans$error <- as.numeric( gsub("insample error= (.*)","\\1",errstr) )
	#body#
	ans$pred <- as.matrix(out[[2]])
	return(ans)
}
