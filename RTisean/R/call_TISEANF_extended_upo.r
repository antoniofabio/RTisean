call_TISEANF_extended_upo <- function(a, bare_options, function_name) # a is a numeric vector, function_name is the TISEAN function name
{
    out=write_to_inputfile(a)
    if (out==1){
	stop("wrong input")
        return()
    }
    tin=input_filename()
    tout=output_filename()
    options=paste(tin," ",bare_options," -V0 -o",tout,"\n", sep="")
    
	toptions=param_filename(0)
	.C("write_string_to_file2",as.character(options),as.integer(0),as.character(toptions),PACKAGE="RTisean")
	.Fortran(function_name,as.character(toptions),PACKAGE="RTisean")
 	txt <- readLines(tout, n = -1, ok = TRUE)
	file.remove(toptions, tin)
	out <- read_TISEAN("")
	attr(out, "txt") <- txt
	return(out)
}

