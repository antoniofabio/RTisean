upo <- function(series,m=-1,r=-1,v=-1,p=1,w=-1,W=-1,a=-1,s=-1,n=-1,l=-1,x=0,c=1){
	options <- ""
	
	if (mode(series)!="numeric")
		stop("wrong input")
	
	if (checkposint(l))
			options <- paste(options," -l",l," ",sep="")	
	else
		if (tneq(l,-1))
			errormessage("l",l)
	
	if (checkint(x))
		options <- paste(options," -x",x," ",sep="")
	else 
		errormessage("x",x)
	
	if (checkcolumns(series,c,1))
		options <- paste(options," -c",c," ",sep="")
	else
		errormessage("c",c)
	
	if (checkposint(m))
		options = paste(options," -m",m," ",sep="")	
	else
		if (!tneq(m,-1))
			stop("m value is not specified")
		else
			errormessage("m",m)
	
	
	if (tneq(v,-1) & tneq(r,-1))
		stop("specifying both r and v is not allowed")
	if (!tneq(v,-1) & !tneq(r,-1) )
		stop("either r or v has to be specified")
	
	if (tneq(r,-1)){
		if (checkpositive(r))
			options <- paste(options," -r",r," ",sep="")	
		else
			errormessage("r",r)
	}
	
	if (tneq(v,-1)){
		if (checkpositive(v))
			options <- paste(options," -v",v," ",sep="")	
		else
			errormessage("v",v)
	}
	
	if (checkposint(p))
		options <- paste(options, " -p",p," ",sep="")
	else
		errormessage("p",p)
	
	if (checkpositive(w))
		options <- paste(options," -w",w," ",sep="")	
	else
		if (tneq(w,-1))
			errormessage("w",w)
	
	if (checkpositive(W))
		options <- paste(options," -W",W," ",sep="")
	else
		if (tneq(W,-1))
			errormessage("W",W)
	
	if (checkpositive(a))
		options = paste(options," -a",a," ",sep="")	
	else
		if (tneq(a,-1))
			errormessage("a",a)
	
	if (checkpositive(s))
		options <- paste(options," -s",s," ",sep="")	
	else
		if (tneq(s,-1))
			errormessage("s",s)
	
	if (checkposint(n))
		options <- paste(options, " -n",n," ",sep="")
	else
		if (tneq(n,-1))
			errormessage("n",n)

	out <- call_TISEANF_extended(series,options,"upo",asIs=TRUE)
	ans <- list()
	nr <- length(out)
	nfields <- 0
	i <- 0
	#FIXME
	repeat {
		i <- i+1
		if(out[i]=="") {
			nfields <- nfields+1
			ans[[nfields]] <- list()
			i <- i+2
			fields <- strsplit(out[i],"  ")[[1]]
			ans[[nfields]]$period <- as.numeric(fields[1])
			ans[[nfields]]$accuracy <- as.numeric(fields[2])
			ans[[nfields]]$stability <- as.numeric(fields[3])
			ans[[nfields]]$orbit <- numeric()
			for(j in (i+1):(i+ans[[nfields]]$period)) {
				tmp <- strsplit(out[j], "  ")[[1]]
				ans[[nfields]]$orbit[j-i] <- as.numeric(tmp[2])
			}
			i <- j
		}
		if(i>=nr) 
			break;
	}
	attr(ans, "txt") <- out
	class(ans) <- "uporesult"
	return(ans)
}

print.uporesult <- function(x, ...)
	cat(attr(x,"txt"),sep="\n")
