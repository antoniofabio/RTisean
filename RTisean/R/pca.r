RT_svd <- function(series,l,x=0,c=1,m=2,d=1,q){
	args <- list(routinename="svd", input=series, x=x, c=c, m=m, d=d)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(q))
		args <- c(args, q=q)
	out <- as.list(do.call(callTISEAN, args))
	eig <- as.numeric(gsub(".* +(.*)","\\1",out[[1]]))
	ans <- list(eigen=eig, project=as.matrix(out[[2]]))
	return(ans)
}
