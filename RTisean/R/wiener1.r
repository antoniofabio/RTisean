wiener1 <- function(series,f=-1,w=-1,l=-1,x=0,c=1){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (checkpositive(f)){
	options = paste(options," -f",d2s(f)," ",sep="")	
}
else{
	if (tneq(f,-1)){
		errormessage("f",f)
		return()
	}
}

if (checkpositive(w)){
	options = paste(options," -w",d2s(w)," ",sep="")	
}
else{
	if (tneq(w,-1)){
		errormessage("w",w)
		return()
	}
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

if (checkcolumns(series,c,1)){
	options=paste(options," -c",c," ",sep="")
}
else{
	errormessage("c",c)
	return()
}

#debug
 

out=call_TISEANF_extended(series,options,"wiener1")
return(out)

}
