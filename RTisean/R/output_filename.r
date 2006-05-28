output_filename <- function(suffix=""){
	ans <- character(length(suffix))
	for(i in 1:length(suffix))
		ans[i] <- paste(tempfile("tout$"),suffix[i],sep="")
	assign(ans, "outfilenames", envir="package:RTisean")
	return(ans)
}
