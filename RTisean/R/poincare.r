poincare <- function(series,l=-1,x=0,c=1,m=2,d=1,q=-1,C=0,a=-1){

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

if (checkint(C)){
    if (C==0 | C==1){
	options = paste(options," -C",i2s(C)," ",sep="")	
    }
    else{
        errormessage("C",C)
        return()
    }
}
else{
	errormessage("C",C)
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


if (checkpositive(a)){
	options = paste(options," -a",d2s(a)," ",sep="")	
}
else{
	if (tneq(a,-1)){
	      errormessage("a",a)
		return()
	}
}


#debug
 

out=call_TISEANC_extended(series,options,"poincare")

colsnum=ncol(out)
namescol=NULL
  for (i in 1:(colsnum-1))
  namescol[i]=paste("dim",i,sep="")
  
namescol[i+1]="time"

colnames(out)=namescol

return(out)

}
