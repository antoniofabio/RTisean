call_TISEANF_extended <- function(a, bare_options, function_name) {
	tin <- write_to_inputfile(a)
	tout <- output_filename()
	options <- paste(tin," ",bare_options," -V0 -o",tout,"\n", sep="")

	toptions <- param_filename(0)
	write(options, toptions)
	call_TISEAN_bare(function_name, toptions)
	out <- read_TISEAN("")
	file.remove(toptions, tin, tout)

	return(out)
}
