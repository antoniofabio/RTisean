project <- function(series,m=-1,q=-1,r=-1,k=-1,i=1,l=-1,x=0,c=1){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (checkposint(m)){
	options = paste(options," -m",i2s(m)," ",sep="")	
}
else{
	if (!tneq(m,-1)){
		print("m value is not specified")
	}
	else{
		errormessage("m",m)
	}
	return()
}

if (checkposint(q)){
	options = paste(options," -q",i2s(q)," ",sep="")	
}
else{
	if (!tneq(q,-1)){
		print("q value is not specified")
	}
	else{
		errormessage("q",q)
	}
	return()
}

if (checkposint(k)){
	options = paste(options," -k",i2s(k)," ",sep="")	
}
else{
	if (!tneq(k,-1)){
		print("k value is not specified")
	}
	else{
		errormessage("k",k)
	}
	return()
}

if (checkpositive(r)){
	options = paste(options," -r",d2s(r)," ",sep="")	
}
else{
	if (!tneq(r,-1)){
		print("r value is not specified")
	}
	else{
		errormessage("r",r)
	}
	return()
}


if (checkposint(i)){
	options = paste(options," -i",i2s(i)," ",sep="")	
}
else{
	errormessage("i",i)
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

#debug
 

out=call_TISEANF_extended(series,options,"project")
return(out)

}
