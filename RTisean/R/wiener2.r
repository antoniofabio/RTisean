wiener2 <- function(series,f=-1,w=-1,o=-1,l=-1,x=0,c=1){
	stop("not yet implemented")

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
