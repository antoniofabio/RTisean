ghkss <- function(series,l=-1,x=0,c=1,m=5,d=1,q=3,k=30,r=-1,i=1,two=FALSE){

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

if (checkposint(q)){
	options = paste(options," -q",i2s(q)," ",sep="")	
}
else{
	errormessage("q",q)
	return()
}

if (checkposint(k)){
	options = paste(options," -k",i2s(k)," ",sep="")	
}
else{
	errormessage("k",k)
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

if (checkposint(i)){
	options = paste(options, " -i",i2s(i)," ",sep="")
}
else{
	errormessage("i",i)
	return()
}

if (two==TRUE){
	options = paste(options, " -2 ",sep="")
}

#debug
 
        suffix = c(1:i)

	for (j in 1:i){
		suffix[j] = paste(".",j,sep="")
	}

       out=call_TISEANC_extended2(series,options,"ghkss",suffix)
       
       out=colnamesout(out,0,"filtered data") #C
       out=out[[1]]                             #C
       
       return(out)

}
