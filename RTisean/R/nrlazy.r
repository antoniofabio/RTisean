nrlazy <- function(series,l,x=0,c=1,m=5,d=1,i=1,r,v){
	args <- list(routinename="lazy", input=series, i=i, x=x, c=c, m=m, d=d)
	if(!missing(r))
		args <- c(args, r=r)
	if(!missing(v))
		args <- c(args, v=v)
	if(!missing(l))
		args <- c(args, l=l)
	if(i>1)
		suffixes <- paste(".",1:i, sep="")
	else
		suffixes=NULL
	args <- c(args, suffixes=suffixes)
	out <- do.call(callTISEAN, args)
	if(i>1)
		out <- lapply(out, as.matrix)
	else
		out <- as.matrix(out)
#	out <- lapply(out, "[", ,1])
	return(out)
}
