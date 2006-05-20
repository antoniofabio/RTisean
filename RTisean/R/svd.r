svd <- function(series,l=-1,x=0,c=1,m=2,d=1,q=-1){

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
	if (tneq(q,-1)){
		errormessage("q",q)
		return()
	}
}



#debug
 

out=call_TISEANC_extended(series,options,"svd")

namesvect=c("eigenval","project") #C and following lines

  if (is.list(out))
  {out[[1]]=chartonum(out[[1]]," ", 2)
  out=prettyout(out,NULL,namesvect)
  }else  out=out[,2]
  
return(out)

}
