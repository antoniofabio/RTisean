#Needs special calling scheme: accepts multiple files as input options
polyback <- function(series,l,x=0,c=1,m=2,d=1,n,s=1,scale=1,p){
	args <- list(routinename="polyback", input=series, x=x, c=c, m=m, d=d, s=s, "#"=scale)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(n))
		args <- c(args, n=n)
	if(missing(p))
		stop("you must pass a 'p' matrix of coefficients")
	args <- c(args, list(parobjects=list(p=p)), remove.extras=TRUE)
	out <- as.data.frame(as.matrix(do.call(callTISEAN, args)))
	names(out) <- c("rem.par","in.err","out.err","removed.a","removed.b")	
	return(out)
}

