c1 <- function(series,d=-1,m=-1,M=-1,t=-1,n=-1,scale=2,K=100,l=-1,x=0,c=1){

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
	
	if (checkcolumns(series,c)){
		options=add_option(options,"c",c)
	}
	else{
		print("wrong input")
		return()
	}
	
	if (checkposint(d)){
		options = paste(options," -d",i2s(d)," ",sep="")	
	}
	else{
		if (!tneq(d,-1)){
			print("d value is not specified");
			return()
		}
		errormessage("d",d)
		return()
	}
	
	if (checkposint(m)){
			options = paste(options," -m",i2s(m)," ",sep="")	
	}
	else{
		if (!tneq(m,-1)){
			print("m value is not specified")
			return()
		}
		errormessage("m",m)
		return()
	}
	
	if (!tneq(M,-1)){
		print("M value is not specified")
		return()
	}
	else{
		if (checkposint(M)){
				if (M<2){
						print("M value must be a positive integer greater than 1")
						return()
				}
				options = paste(options," -M",i2s(M)," ",sep="")
		}
		else{
				errormessage("M",M)
				return()
		}
	}
	
	
	if (checkposint(t)){
			options = paste(options," -t",i2s(t)," ",sep="")	
	}
	else{
		if (!tneq(t,-1)){
			print("t value is not specified")
			return()
		}
		errormessage("t",t)
		return()
	}
	
	
	if (checkposint(n)){
			options = paste(options," -n",i2s(n)," ",sep="")	
	}
	else{
		if (!tneq(n,-1)){
			print("n value is not specified")
			return()
		}
		errormessage("n",n)
		return()
	}
	
	
	if (checkposint(scale)){
		options = paste(options, " -#",i2s(scale)," ",sep="")
	}
	else{
		errormessage("scale",scale)
		return()
	}
	
	
	if (checkposint(K)){
			options = paste(options," -K",i2s(K)," ",sep="")	
	}
	else{
		errormessage("K",K)
		return()
	}

	out <- call_TISEANF_extended(series,options,"c1",split=TRUE)[[1]]
	return(out)
}
