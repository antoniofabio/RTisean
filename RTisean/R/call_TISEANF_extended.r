#taken bare options and ordinal R object input, calls TISEAN routine by means of temp input/output files
call_TISEANF_extended <- function(a, bare_options, function_name) {
	#one input file, one output file
	tin <- write_to_inputfile(a)
	tout <- output_filename()
	options <- paste(tin," ",bare_options," -V0 -o",tout,"\n", sep="")
	call_TISEAN_bare(options, function_name)
	out <- read_TISEAN(tout)
	file.remove(tin, tout)
	return(out)
}
