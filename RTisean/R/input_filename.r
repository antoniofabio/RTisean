input_filename <- function() {
	ans <- tempfile("tin$")
	assign(ans, "inputfilenames", envir=as.environment("package:RTisean"))
	return(ans)
}