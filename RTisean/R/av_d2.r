av_d2 <- function(lst,m=1,M=-1,a=1,E=FALSE){

options = ""

if (!is_nice_list(lst)){
    print("wrong input")
    return()
}

if (checkposint(m)){
	options = paste(options," -m",m," ",sep="")	
} 
else{
	errormessage("m",m)
	return()
}

if (checkposint(M)){
		options = paste(options," -M",M," ",sep="")	
}
else{
	if (tneq(M,-1)){
		errormessage("M",M)
		return()
	}
}

if (checkposint(a)){
	options = paste(options," -a",i2s(a)," ",sep="")	
} 
else{
	errormessage("a",a)
	return()
}

if (E==TRUE){
	options = paste(options," -E ",sep="")	
}

out=call_TISEANC_extended(lst,options,"av-d2")

out=colnamesout(out,0,c("epsilon","smoothcorrdim"))  #C

return(out)

}
