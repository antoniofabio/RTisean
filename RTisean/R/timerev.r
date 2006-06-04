#Output has to be explicitely redirected to a temp file
timerev <- function(series,d=1,l,x=0,c=1) {
	args <- list(d=d, x=x,c=c, V=0)
	if(!missing(l))
		args <- c(args, l=l)

	.serialize(series, tin <- .getTempFName())
	tout <- .getTempFName()
	if(exists(".TISEANpath"))
		routinename <- file.path(.TISEANpath, routinename)
	cmd <- paste("timerev"," ",.listToOpts(args), " ",tin," > ",tout,sep="")
	try(system(cmd, intern = FALSE))
	out <- readLines(tout)
	out <- as.numeric(gsub(" *(.*) .*","\\1",out))
	file.remove(tin,tout)
	return(out)
}
