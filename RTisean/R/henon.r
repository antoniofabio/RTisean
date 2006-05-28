henon <- function(l=-1,A=1.4,B=0.3,X=-1,Y=-1,x=10000){

	options <- ""
	
	if (checkposint(l)){
			options = paste(options," -l",i2s(l)," ",sep="")	
	}
	else{
			if (tneq(l,-1)){
				errormessage("l",l)
				return()
			}
			else{
				print("l value is not specified")
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
	
	if (checknonneg(A)){
		options = paste(options," -A",d2s(A)," ",sep="")		
	}
	else{
		errormessage("A",A)
		return()	
	}
	
	if (checknonneg(B)){
		options = paste(options," -B",d2s(B)," ",sep="")		
	}
	else{
		errormessage("B",B)
		return()	
	}
	
	if (checknonneg(X)){
		options = paste(options," -X",d2s(X)," ",sep="")		
	}
	else{
		if (tneq(X,-1)){
			errormessage("X",X)
			return()	
		}
	}
	
	if (checknonneg(Y)){
		options = paste(options," -Y",d2s(Y)," ",sep="")		
	}
	else{
		if (tneq(Y,-1)){	
			errormessage("Y",Y)
			return()
		}	
	}
	
	series <- 0 #dummy
	out <- call_TISEANF_extended(series,options,"henon")
	return(as.matrix(out[[1]]))
}
