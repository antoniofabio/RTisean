rbf <- function(series,l=-1,x=0,c=1,m=2,d=1,p=10,X=FALSE,s=1,n=-1,L=-1){

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

if (checkposint(p)){
	options = paste(options," -p",i2s(p)," ",sep="")	
}
else{
	errormessage("p",p)
	return()
}

if (X==FALSE){
	options = paste(options," -X ",sep="")	
}


if (checkposint(n)){
		options = paste(options," -n",i2s(n)," ",sep="")	
}
else{
	if (tneq(n,-1)){
		errormessage("n",n)
		return()
	}
}


if (checkint(L)){
	options = paste(options," -L",i2s(L)," ",sep="")	
}
else{
    if (tneq(L,-1)){
	errormessage("L",L)
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
 

outtmp=call_TISEANC_extended(series,options,"rbf")

out=list()
outtmp1=outtmp[[1]][2:(p+1)]
outtmplength=length(outtmp[[1]])

out[[1]]=cbind(chartonum(outtmp1," ", 2),chartonum(outtmp1," ", 3))
out[[2]]=chartonum(outtmp[[1]][p+2]," ",2)
out[[3]]=chartonum(outtmp[[1]][(p+4):(outtmplength-1)],"#",2)
out[[4]]=chartonum(outtmp[[1]][outtmplength]," ",3)

  if(L>0)
  {out[[5]]=outtmp[[2]]
  out=prettyout(out,NULL,c("centers","variance","coeff","error","pred"))
  } else
  out=prettyout(out,NULL,c("centers","variance","coeff","error"))

return(out)

}
