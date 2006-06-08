d2 <- function(series,l,x=0,d=1,M,c,t=0,R,r,scale=100,N=1000,E=FALSE, pretty=FALSE){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list()
	if(!missing(l))
		args <- concat(args,list(l=l))
	if(E)
		args <- concat(args, list("E"))
	if(!missing(M))
		args <- concat(args, list(M=M))
	if(!missing(R))
		args <- concat(args, list(R=R))
	if(!missing(r))
		args <- concat(args, list(r=r))
	if(!missing(c))
		args <- concat(args, list(c=c))
	args <- concat(args, list(routinename="d2",input=series, x=x, d=d, t=t, "#"=scale, N=N, 
		suffixes=concat(".c2",".d2",".h2",".stat")))
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- do.call(callTISEAN, args)[1:3]
	out <- lapply(out, as.list)
	out <- lapply(out, as.matrixList)

	out[[1]] <- colnamesout(out[[1]], 1, c("epsilon","integral"))
	out[[1]][[1]] <- out[[1]][[1]][2]
	out[[2]] <- colnamesout(out[[2]], 1, c("epsilon","dimension"))
	out[[2]][[1]] <- out[[2]][[1]][2]
	out[[3]] <- colnamesout(out[[3]], 1, c("epsilon","entropy"))
	out[[3]][[1]] <- out[[3]][[1]][2]

	if(pretty)
		out <- lapply(out, .cleanList)

	return(out)
}
