param_filename <- function(ordinals){
	out <- character(length(ordinals))
	for (i in 1:length(ordinals))
		out[i] <- paste(tempfile("tparam"),ordinals[i],sep="")
	assign(out, "paramfilenames", envir=as.environment("package:RTisean"))
	return(out)
}
