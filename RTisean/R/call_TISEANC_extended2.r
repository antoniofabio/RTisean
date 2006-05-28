call_TISEANC_extended2 <- function(a, options, function_name, suffix, asIs=FALSE, split=FALSE) {
# adresses the case of one input file and several output files (whose suffixes are given in the vector suffix)

	options <- paste(" ",options," -V0 ",sep="")
	tin <- write_to_inputfile(a)
	tout <- output_filename()
	options <- paste(tin," ",options," -o",tout, sep="")

	call_TISEAN_bare(options, function_name)
	if(!asIs)
		out <- lapply(read_TISEAN(paste(tout,suffix,sep=""), split=split),as.matrix)
	else
		out <- readLines(tout)
	file.remove(tin, paste(tout,suffix,sep=""))
	names(out) <- suffix
	return(out)
}
