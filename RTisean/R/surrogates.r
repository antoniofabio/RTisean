surrogates <- function(series,n=1,i=-1,S=FALSE,I=-1,l=-1,x=0,m=-1,c=-1){

#-------------------------  includes multiple components options --------------------------


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
	options=add_option(options,"c",c)
}
else{
	print("wrong input")
	return()
}

if (checkposint(n)){
	options = paste(options," -n",i2s(n)," ",sep="")	
}
else{
	errormessage("n",n)
	return()
}

if (checkposint(i)){
		options = paste(options," -i",i2s(i)," ",sep="")	
}
else{
	if (tneq(i,-1)){
		errormessage("i",i)
		return()
	}
}

if (S==TRUE){
	options=paste(options," -S ",sep="")
}

if (checkint(I)){
	options=paste(options," -I",i2s(I)," ",sep="")
}
else{
	if (tneq(I,-1)){
		errormessage("I",I)
		return()
	}
}

#debug
 

out=call_TISEANF_extended(series,options,"surrogates")
return(out)

}
