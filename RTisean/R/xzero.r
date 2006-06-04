xzero <- function(series,l,x=0,c,m=3,d=1,n,k=30,r,f=1.2,s=1){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="xzero", input=series, x=x, m=m, d=d, k=k, f=f, s=s)
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(c))
		args <- concat(args, c=c)
	if(!missing(n))
		args <- concat(args, n=n)
	if(!missing(r))
		args <- concat(args, r=r)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- as.matrix(do.call(callTISEAN, args))
	colnames(out) <- c("step","rel.error")
	return(out)	
}
