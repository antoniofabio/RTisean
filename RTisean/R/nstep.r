nstep <- function(series,l=-1,x=0,m=-1,c=-1,d=1,L=1000,k=30,r=-1,f=1.2,O=FALSE){

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



if (checkcolumnsinput(series,c,m,2)){
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


if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}
else{
	errormessage("d",d)
	return()
}

if (checkint(L)){
		options = paste(options," -L",i2s(L)," ",sep="")	
}
else{
		errormessage("L",L)
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

if (checkpositive(f)){
	options = paste(options, " -f",d2s(f)," ",sep="")
}
else{
	errormessage("f",f)
	return()
}

if (O==TRUE){
	options = paste(options, " -O",sep="")
}



#debug
 

out=call_TISEANC_extended(series,options,"nstep")

colnames(out)="predictions" #C
return(out)

}
