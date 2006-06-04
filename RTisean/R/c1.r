c1 <- function(series,d=-1,m=-1,M=-1,t=-1,n=-1, scale=2, K=100,l,x=0,c=1, pretty=FALSE){
	if(missing(l))
		out <- callTISEAN("c1",input=series,d=d, m=m, M=M, t=t, n=n, scale, K=K, x=x,c=c)
	else
		out <- callTISEAN("c1",input=series,d=d, m=m, M=M, t=t, n=n, scale, l=l, K=K, x=x,c=c)
	out <- as.list(out)
	tmpn <- length(out)/2
	out[(1:tmpn)*2] <- lapply(out[(1:tmpn)*2], as.matrix)
	out <- colnamesout(out,1,c("radius","mass"))
	if(pretty) {
		#gsub("[^0-9]*([0-9]+)$","\\1",s) for extracting embedding dimensions from labels
		nms <- unlist(out[(1:tmpn-1)*2+1])
		out <- out[(1:tmpn)*2]
		names(out) <- nms
	}
	return(out)
}
