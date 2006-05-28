#taken bare options and ordinal R object input, calls TISEAN routine by means of temp input/output files
call_TISEANF_extended <- function(a, bare_options, function_name, asIs=FALSE, inputAsIs=FALSE, split=FALSE) {
	#one input file, one output file
	if(!inputAsIs)
		tin <- write_to_inputfile(a)
	else {
		tin <- input_filename()
		writeLines(a, tin)
	}
	tout <- output_filename()
	options <- paste(tin," ",bare_options," -V0 -o",tout,"\n", sep="")
	call_TISEAN_bare(options, function_name)
	if(!asIs)
		out <- read_TISEAN(tout, split=split)
	else
			out <- readLines(tout)
	file.remove(tin, tout)
	return(out)
}
