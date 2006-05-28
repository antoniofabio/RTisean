RT_delay <- function(series,d=1,m=2,l=-1,x=0,c=1){
	options <- ""
	if (mode(series)!="numeric")
		stop("wrong input")
	options <- paste("-d",d," -m",m," -l",l, " -x",x, " -c",c, sep="")
	out <- call_TISEANF_extended(series,options,"delay")
	return(out)
}
