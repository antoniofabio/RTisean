surrogates <- function(series,n=1,i,S=FALSE,I,l,x=0,m,c){
	concat <- get("c",envir=as.environment("package:base"))
	args <- list(routinename="surrogates", input=series, n=n, x=x)
	if(!missing(i))
		args <- c(args, i=i)
	if(!missing(I))
		args <- c(args, I=I)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(m))
		args <- c(args, m=m)
	if(!missing(c))
		args <- c(args, c=c)
	if(S)
		args <- c(args, "S")
	if(missing(c)) {c <- get("c",envir=as.environment("package:base"))}
	out <- as.matrix(do.call(callTISEAN, args))
	return(out)
}
