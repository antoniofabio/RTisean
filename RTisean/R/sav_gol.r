sav_gol <- function(series,l,x=0,c,m,n="2,2",p=2,D=0){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="sav_gol", input=series,x=x, n=n, p=p, D=D)
	if(!missing(l))
		args <- concat(args, l)
	if(!missing(c))
		args <- concat(args, c)
	if(!missing(m))
		args <- concat(args, m)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}

	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
