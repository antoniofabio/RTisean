RT_pca <- function(series,l,x=0,c=1,m=c(1,2),d=1, W=0, q){
	args <- list(routinename="pca", input=series, x=x, c=c, m=m, d=d, W=W)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(q))
		args <- c(args, q=q)
	return(as.matrix(do.call(callTISEAN, args)))
}

RT_svd <- RT_pca
