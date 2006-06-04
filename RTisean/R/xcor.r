xcor <- function(series,l,x=0,c,D=100){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="xcor", input=series, x=x, D=D)
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(c))
		args <- concat(args, c=c)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- as.matrixList(as.list(do.call(callTISEAN, args)))
	out <- colnamesout(out,1,c("lag","(cross corr.)/sd") )
	out <- out[[2]]
	return(out)
}
