polynom <- function(series,l=-1,x=0,c=1,m=2,d=1,p=2,n=-1,L=-1){

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

if (checkint(p)){
	options = paste(options," -p",i2s(p)," ",sep="")	
}
else{
	errormessage("p",p)
	return()
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
		if (!tneq(L,-1)){
			print("L value is not specified")
		}
		else{
			errormessage("L",L)
		}
		return()
}

out=call_TISEANC_extended(series,options,"polynom")

out[[2]]= chartonum(out[[2]][-1]," ",3)   #C and following lines
out[[3]]= chartonum(out[[3]],"= ", 2)
 if (length(out[[3]]==2)) names(out[[3]])=c("avg.in.error","avg.out.error") else
 names(out[[3]])="avg.in.error"


 if (length(out)==4) out=prettyout(out,1,c("coeff","err","pred")) else
 out=prettyout(out,1,c("coeff","err"))

return(out)

}
