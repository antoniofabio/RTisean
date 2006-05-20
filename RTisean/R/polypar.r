polypar <- function(m=2,p=3){

options = ""

#TODO: insert here a call to validate lst format

if (checkposint(m)){
	options = paste(options," -m",i2s(m)," ",sep="")	
} 
else{
	errormessage("m",m)
	return()
}

if (checkposint(p)){
		options = paste(options," -p",i2s(p)," ",sep="")	
}
else{
	errormessage("p",p)
	return()
}

tout=output_filename()

options = paste(options," -V0 -o",tout)
.C("call_TISEAN_bare",as.character(options),as.character("polypar"),PACKAGE="RTisean")
out = read_TISEAN("")
return(out)

}
