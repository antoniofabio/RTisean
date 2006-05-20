ll_ar <- function(series,l=-1,x=0,c=1,m=2,d=1,i=-1,r=-1,R=-1,f=1.2,s=1,C=-1){

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
	if (tneq(i,-1)){
		errormessage("i",i)
		return()
	}
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

if (checkpositive(R)){
	options = paste(options," -R",d2s(R)," ",sep="")	
}
else{ 
	if (tneq(R,-1)){
	      errormessage("R",R)
		return()
	}
}


if (checkpositive(f)){
	options = paste(options, " -f",d2s(f)," ",sep="")
}
else{
	errormessage("f",f)
	return()
}

if (checkposint(s)){
	options = paste(options, " -s",i2s(s)," ",sep="")
}
else{
	errormessage("s",s)
	return()
}


if (checknonneg(C)){
	options = paste(options, " -C",d2s(C)," ",sep="")
}
else{
	if (tneq(C,-1)){
		errormessage("C",C)
		return()
	}
}

#debug
 

out=call_TISEANC_extended(series,options,"ll-ar")



  if( is.list(out)){  #C and following lines
  out=colnamesout(out,1,c("neigh.size","rel.error", "fraction", "avg.neigh.", "variance" ))   
  out=out[[2]] 
  
  return(out)
  } else return(print("singular matrix, no result returned"))

}
