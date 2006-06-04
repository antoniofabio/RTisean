c1 <- function(series,d,m,M,t,n, scale=2, K=100,l,x=0,c=1, pretty=FALSE){
	args <- list("c1", input=series, K=K, x=x, c=c, "#"=scale)
	if(!missing(d))
		args <- c(args, d=d)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(M))
		args <- c(args, M=M)
	if(!missing(t))
		args <- c(args, t=t)
	if(!missing(n))
		args <- c(args, n=n)
	if(!missing(l))
		args <- c(args, l=l)

	out <- do.call(callTISEAN, args)
	out <- as.matrixList(as.list(out))
	out <- colnamesout(out[-length(out)],1,c("radius","mass"))
	if(pretty) {
		nms <- unlist(out[(1:length(out)-1)*2+1])
		out <- out[(1:length(nms))*2]
		names(out) <- nms
	}
	return(out)
}
