lzo.test <- function(series,l,x=0,m=c(1,2),c,d=1,n,S=1,k=30,r,f=1.2,s=1,C){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="lzo-test", input=series, x=x, d=d, S=S,k=k,f=f,s=s)
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(m))
		args <- concat(args, m=m)
	if(!missing(c))
		args <- concat(args, c=c)
	if(!missing(n))
		args <- concat(args, n=n)
	if(!missing(r))
		args <- concat(args, r=r)
	if(!missing(C))
		args <- concat(args, C=C)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}

zeroth <- lzo.test
