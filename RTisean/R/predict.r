predict <- function(series,d=-1,m=-1,r=-1,v=-1,s=1,l=-1,x=0,c=1){

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

if (tneq(v,-1) & tneq(r,-1)){
	print("specifying both r and v is not allowed")
	return()
}
if ( !tneq(v,-1) & !tneq(r,-1) ){
	print("either r or v has to be specified")
	return()
}

if (tneq(r,-1)){
	if (checkpositive(r)){
		options = paste(options," -r",d2s(r)," ",sep="")	
	}
	else{ 	
		errormessage("r",r)
		return()
	}
}

if (tneq(v,-1)){
	if (checkpositive(v)){
		options = paste(options," -v",d2s(v)," ",sep="")	
	}
	else{ 	
		errormessage("v",v)
		return()
	}
}


if (checkposint(s)){
	options = paste(options, " -s",i2s(s)," ",sep="")
}
else{
	errormessage("s",s)
	return()
}



#debug
 

out=call_TISEANF_extended(series,options,"predict")
return(out)

}
