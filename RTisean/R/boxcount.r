boxcount <- function(series,l,x=0,c,d=1,M,Q=2.0,R,r,scale=20){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="boxcount", input=series, x=x, d=d, Q=Q, "#"=scale)
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(c))
		args <- concat(args, c=c)
	if(!missing(M))
		args <- concat(args, M=M)
	if(!missing(R))
		args <- concat(args, R=R)
	if(!missing(r))
		args <- concat(args, r=r)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	ans <- as.matrixList( as.list( do.call(callTISEAN, args) ) )
	nl <- floor(length(ans)/2)
	nms <- ans[(0:(nl-1))*2+1]
	ans <- ans[(1:nl)*2]
	ans <- colnamesout(ans,0,c("epsilon","entropy","diff. entropy"))
	names(ans) <- nms
	return(ans)
}
