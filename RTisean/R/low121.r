low121 <- function(series,l=-1,x=0,c=1,i=1){

options = ""

if (mode(series)!="numeric")
	stop("wrong input")

if (checkposint(l)){
	options = paste(options," -l",i2s(l)," ",sep="")	
}
else{
	if (tneq(l,-1)){
		errormessage("l",l)
		return()
	}
}

if (checkint(x)){
	options = paste(options," -x",i2s(x)," ",sep="")	
}
else{
	errormessage("x",x)
	return()
}

if (checkcolumns(series,c,1)){
	options=paste(options," -c",c," ",sep="")
}
else{
	errormessage("c",c)
	return()
}

if (checkposint(i)){
	options = paste(options, " -i",i2s(i)," ",sep="")
}
else{
	errormessage("i",i)
	return()
}

#debug
 

out=call_TISEANC_extended2(series,options,"low121",c(".1"))

out=out[[1]]    #C

return(out)

}
