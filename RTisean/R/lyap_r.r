lyap_r <- function(series,l=-1,x=0,c=1,m=2,d=1,t=0,r=-1,s=50){

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

if (checkint(t)){
	options = paste(options, " -t",i2s(t)," ",sep="")
}
else{
	errormessage("t",t)
	return()
}


if (checkpositive(r)){
	options = paste(options," -r",d2s(r)," ",sep="")	
}
else{ 
	if (tneq(r,-1)){
	      errormessage("r",r)
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
 

out=call_TISEANC_extended(series,options,"lyap_r")

colnames(out)=c("iteration","log.stretch") #C
return(out)

}
