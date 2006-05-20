nrlazy <- function(series,l=-1,x=0,c=1,m=5,d=1,i=1,r=-1,v=-1){

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
	errormessage("m",m)
	return()
}

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}
else{
	errormessage("d",d)
	return()
}

if (checkposint(i)){
		options = paste(options," -i",i2s(i)," ",sep="")	
}
else{
		errormessage("i",i)
		return()
}


if (tneq(v,-1) & tneq(r,-1)){
	print("specifying both r and v is not allowed")
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

#debug
        suffix = c(1:i)

	for (j in 1:i){
		suffix[j] = paste(".",j,sep="")
	}


       out=call_TISEANC_extended2(series,options,"nrlazy",suffix)
       
       out=colnamesout(out,0,"filtered data") #C
       out=out[[1]] #C
       
       return(out)

}
