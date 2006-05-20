sav_gol <- function(series,l=-1,x=0,c=-1,m=-1,n=c(2,2),p=2,D=0){

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

if (checkcolumnsinput(series,c,m,1)){
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

if (is_vector(n)){
	if (length(n)!=2 | !checkposint(n[1]) | !checkposint(n[2])){
		errormessage("n",n)
		return()
	}
	else{
		options=add_option(options,"n",n)
	}
}
else{
	errormessage("n",n)
	return()
}	

if (checkposint(p)){
	options = paste(options," -p",i2s(p)," ",sep="")	
}
else{
	errormessage("p",p)
	return()
}

if (checkint(D)){
	options = paste(options," -D",i2s(D)," ",sep="")	
}
else{
	errormessage("D",D)
	return()
}

#debug
 

out=call_TISEANC_extended(series,options,"sav_gol")
return(out)

}
