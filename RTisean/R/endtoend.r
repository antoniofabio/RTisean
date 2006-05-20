endtoend <- function(series,l=-1,x=0,m=-1,c=1){

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

#debug
 
outtmp=call_TISEANF_extended(series,options,"endtoend")

out=list()        #C and following lines


  for(i in 1:length(outtmp) )
  {out[[i]]=list()
  
  firsto=strsplit(outtmp[[i]][1],":")
  out[[i]]$length= chartonum(firsto[[1]][2],"offset", 1)
  out[[i]]$offset=chartonum(firsto[[1]][3],"lost",1)
  out[[i]]$lost=chartonum(firsto[[1]][4],"%\n", 1)
  
  secondo=strsplit(outtmp[[i]][2],":")
  out[[i]]$jump=chartonum(secondo[[1]][2],"%\n",1)
  
  thirdo=strsplit(outtmp[[i]][3],":")
  out[[i]]$slip=chartonum(thirdo[[1]][2],"%\n",1)
  
  fourtho=strsplit(outtmp[[i]][4],":")
  out[[i]]$weighted= chartonum(fourtho[[1]][2],"%\n",1)
  }
  
return(out)

}
