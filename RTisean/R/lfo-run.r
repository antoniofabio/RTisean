lfo.run <- function(series,l,x=0,m,c,d=1,L=1000,k=30,r,f=1.2,O=FALSE){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="lfo-run", input=series, x=x,d=d,L=L,k=k,f=f)
	if(O)
		args <- concat(args, list("O"))
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(m))
		args <- concat(args, m=m)
	if(!missing(c))
		args <- concat(args, c=c)
	if(!missing(r))
		args <- concat(args, r=r)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- as.matrix(do.call(callTISEAN, args))
	colnames(out) <- "predictions"
	return(out)
}

nstep <- lfo.run