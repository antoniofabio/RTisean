#Output has to be explicitely redirected to a temp file
timerev <- function(series,d=1,l,x=0,c=1) {
	args <- list(routinename="timerev", input=series, d=d, x=x,c=c)
	if(!missing(l))
		args <- c(args, l=l)
	args <- c(args, noout=TRUE)

	out <- as.character(do.call(callTISEAN,args))
	out <- as.numeric(gsub(" *(.*) .*","\\1",out))
	return(out)
}
