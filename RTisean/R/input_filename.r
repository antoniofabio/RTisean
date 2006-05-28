input_filename <- function() {
	ans <- tempfile("tin$")
	assign(ans, "inputfilenames", envir="package:RTisean")
	return(ans)
}