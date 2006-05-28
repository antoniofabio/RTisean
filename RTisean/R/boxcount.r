boxcount <- function(series,l=-1,x=0,c=-1,d=1,M=-1,Q=2.0,R=-1,r=-1,scale=20){

options <- ""

if (mode(series)!="numeric")
	stop("wrong input")

if (checkposint(l))
		options = paste(options," -l",i2s(l)," ",sep="")	
else
	if (tneq(l,-1))
		errormessage("l",l)

if (checkint(x))
	options <- paste(options," -x",i2s(x)," ",sep="")	
else
	errormessage("x",x)

if (checkcolumnsinput(series,c,M,2)){
	if (tneq(c,-1)){
		options=add_option(options,"c",c)
	}
	if (tneq(M,-1)){
		options=add_option(options,"M",M)
	}
}     else
	stop("wrong input")

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}  else{
	errormessage("d",d)
}

if (checkpositive(Q)){
	options = paste(options," -Q",d2s(Q)," ",sep="")	
}    else{
	errormessage("Q",Q)
}

if (checkpositive(r)){
	options = paste(options," -r",d2s(r)," ",sep="")	
} else{ 
	if (tneq(r,-1)){
	      errormessage("r",r)
	}
}

if (checkpositive(R)){
	options = paste(options," -R",d2s(R)," ",sep="")	
} else{ 
	if (tneq(R,-1)){
	      errormessage("R",R)
	}
}

if (checkposint(scale)){
	options = paste(options, " -#",scale," ",sep="")
} else{
	errormessage("scale",scale)
}

out <- call_TISEANC_extended(series,options,"boxcount",asIs=TRUE)

return(out)

}

