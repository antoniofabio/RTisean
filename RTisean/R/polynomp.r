polynomp <- function(series,l=-1,x=0,c=1,m=2,d=1,n=-1,L=1000,p=-1){

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

if (checkposint(n)){
		options = paste(options," -n",i2s(n)," ",sep="")	
}
else{
	if (tneq(n,-1)){
		errormessage("n",n)
		return()
	}
}


if (checkposint(L)){
	options = paste(options, " -L",i2s(L)," ",sep="")
}
else{
	errormessage("L",L)
	return()
}

#debug

if (!is_matrix(p)){
	print("wrong input")
        return()
}
else{
    if (dim(p)[2]!=2 | tneq(p,round(p)) ){
        print("wrong input")
        return()
    }
}

param=list()
param[[1]]=p
outtmp=call_TISEANC_extended3(series,options,"polynomp","",param,"p")

out=list()                #C and following lines
out[[1]]= chartonum(unlist(outtmp[[1]][1])[-1] ," ",4)
out[[2]]= unlist(outtmp[[1]][2])

out=prettyout(out,NULL,c("coeff","pred"))

return(out)

}
