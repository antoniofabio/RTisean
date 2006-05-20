recurr <- function(series,l=-1,x=0,c=-1,m=-1,d=1,r=-1,percent=100){

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

if (checkcolumnsinput(series,c,m,2)){
	if (tneq(c,-1)){
		options=add_option(options,"c",c)
	}
	if (tneq(m,-1)){
		options=add_option(options,"m",m)
	}
}
else{
	print("wrong input")
	return()
}

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}
else{
	errormessage("d",d)
	return()
}

if (checkpositive(r)){
	options=paste(options," -r",d2s(r)," ",sep="")
}
else{
	if (tneq(r,-1)){
		errormessage("r",r)
		return()
	}
}

if (checknonneg(percent)){
	if (percent>100){
		errormessage("percent",percent)
		return()
	}
	options=paste(options," -%",percent," ",sep="")
}
else{
	errormessage("percent",percent)
	return()
}

#debug
 

out=call_TISEANC_extended(series,options,"recurr")
return(out)

}
