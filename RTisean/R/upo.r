upo <- function(series,m=-1,r=-1,v=-1,p=1,w=-1,W=-1,a=-1,s=-1,n=-1,l=-1,x=0,c=1,pretty=FALSE){ #C

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


if (tneq(v,-1) & tneq(r,-1)){
	print("specifying both r and v is not allowed")
	return()
}
if (!tneq(v,-1) & !tneq(r,-1) ){
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

if (checkposint(p)){
	options = paste(options, " -p",i2s(p)," ",sep="")
}
else{
	errormessage("p",p)
	return()
}


if (checkpositive(w)){
	options = paste(options," -w",d2s(w)," ",sep="")	
} 
else{
	if (tneq(w,-1)){
		errormessage("w",w)
		return()
	}
}

if (checkpositive(W)){
	options = paste(options," -W",d2s(W)," ",sep="")	
} 
else{
	if (tneq(W,-1)){
		errormessage("W",W)
		return()
	}
}

if (checkpositive(a)){
	options = paste(options," -a",d2s(a)," ",sep="")	
} 
else{
	if (tneq(a,-1)){
		errormessage("a",a)
		return()
	}
}


if (checkpositive(s)){
	options = paste(options," -s",d2s(s)," ",sep="")	
} 
else{
	if (tneq(s,-1)){
		errormessage("s",s)
		return()
	}
}


if (checkposint(n)){
	options = paste(options, " -n",i2s(n)," ",sep="")
}
else{
	if (tneq(n,-1)){
		errormessage("n",n)
		return()
	}
}

#debug
 

	out <-call_TISEANF_extended_upo(series,options,"upo")

  if(pretty)  #C and following lines
  {out=cleanfinalout(out,1) 
  out=colnamesout(out,0,c("period","accuracy","stability"))
  }
  
return(out)

}
