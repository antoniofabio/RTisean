wiener2 <- function(series,f=-1,w=-1,o=-1,l=-1,x=0,c=1){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (!is_matrix(o)){
	print("wrong input")
	return()
}
if (dim(o)[2]!=2){
	print("wrong input")
	return()
}

if (checkpositive(f)){
	options = paste(options," -f",d2s(f)," ",sep="")	
}
else{
	if (tneq(f,-1)){
		errormessage("f",f)
		return()
	}
}

if (checkpositive(w)){
	options = paste(options," -w",d2s(w)," ",sep="")	
}
else{
	if (tneq(w,-1)){
		errormessage("w",w)
		return()
	}
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

#debug
 

   tpar=param_filename(1)
   write_to_file(o,tpar)	
   options = paste(" ",options," -o",tpar," -V0 ",sep="")

    out=write_to_inputfile(series)
    if (out==1){
	print("wrong input")
        return()
    }
    tin=input_filename()
    tout=output_filename()
    options=paste(tin,options," -V0 -O",tout)
    
	 toptions=param_filename(0)
	.C("write_string_to_file2",as.character(options),as.integer(0),as.character(toptions),PACKAGE="RTisean")
        .Fortran("wiener2",as.character(toptions),PACKAGE="RTisean")
	.C("delete_file",as.character(toptions),PACKAGE="RTisean")
	.C("delete_file",tpar,PACKAGE="RTisean")
        .C("delete_file",as.character(tin),PACKAGE="RTisean")
	
   	out = read_TISEAN("")
   
   return(out)
}
