upoembed <- function(series,d=-1,m=2,p=1,l=-1,x=0,c=1){

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

if (checkcolumns(series,c,1)){
	options=paste(options," -c",c," ",sep="")
}
else{
	errormessage("c",c)
	return()
}

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
} 
else{
	if (!tneq(d,-1)){
		print("d value is not specified")
	}
	else{
		errormessage("d",d)
	}
	return()
}


if (checkposint(m)){
	options = paste(options," -m",i2s(m)," ",sep="")	
} 
else{
	errormessage("m",m)
	return()
}

if (checkposint(p)){
	options = paste(options, " -p",i2s(p)," ",sep="")
}
else{
	errormessage("p",p)
	return()
}



#debug
 

out=call_TISEANF_extended(series,options,"upoembed")
return(out)

}
