#Output has to be explicitely redirected to a temp file
onestep <- function(series,l,x=0,c,m=2,d=1,n,k=30,r,f=1.2,s=1,C){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="onestep", input=series, x=x, m=m, d=d,k=k,f=f,s=s)
	if(!missing(l))
		args <- concat(args, l=l)
	if(!missing(c))
		args <- concat(args, c=c)
	if(!missing(n))
		args <- concat(args, n=n)
	if(!missing(r))
		args <- concat(args, r=r)
	if(!missing(C))
		args <- concat(args, C=C)
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	args <- c(args, noout=TRUE)
	out <- as.character(do.call(callTISEAN,args))
	out <- as.numeric(gsub(".*= (.*)","\\1",out))
	return(out)
}
