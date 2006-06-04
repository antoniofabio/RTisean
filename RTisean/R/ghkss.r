ghkss <- function(series,l,x=0,c=1,m=5,d=1,q=3,k=30,r,i=1,two=FALSE){
	args <- list(routinename="ghkss", input=series, x=x, c=c, m=m, d=d, q=q, k=k, i=i)
	if(two)
		args <- c(args, 2)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(r))
		args <- c(args, r=r)

	suffixes <- paste(".",1:i,sep="")
	args <- c(args, suffixes=suffixes)
	out <- do.call(callTISEAN,args)
	out <- lapply(out, as.matrix)
	return(out)
}
