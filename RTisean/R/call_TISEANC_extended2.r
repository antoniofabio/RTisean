call_TISEANC_extended2 <- function(a, options, function_name,suffix) # a is a numeric vector, function_name is the TISEAN function name
{
# adresses the case of one input file and several output files (whose suffixes are given in the vector suffix)

	options <- paste(" ",options," -V0 ",sep="")
	tin <- input_filename()
	tout <- output_filename()
	options <- paste(tin," ",options," -o",tout, sep="")
	
	out <- write_to_inputfile(a)
	if (out==1)
		stop("wrong input")
	
	out <- .C("call_TISEAN_bare", as.character(options), as.character(function_name),PACKAGE="RTisean")
	file.remove(input_filename())
	out <- list()
	for (i in 1:length(suffix))
		out[[i]] <- read_TISEAN(suffix[i])
	
	return(out)
}
