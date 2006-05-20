xcor <- function(series,l=-1,x=0,c=-1,D=100){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

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

if (c==-1){
	c=c(1,2)
}
if (checkcolumns(series,c,2)){
	options=add_option(options,"c",c)
}
else{
	print("wrong input")
	return()
}


if (checkposint(D)){
	options=paste(options," -D",i2s(D)," ",sep="")
}
else{
	errormessage("D",D)
	return()
}

#debug

out=call_TISEANC_extended(series,options,"xcor")

out=colnamesout(out,1,c("lag","(cross corr.)/sd") )

out=out[[2]]

return(out)

}
