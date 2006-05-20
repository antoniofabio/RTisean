output_filename <- function(suffix=""){
	out=character(length(suffix))
	for (i in 1:length(suffix)){
		tout=.C("Routput_filename",tout=character(1),PACKAGE="RTisean")$tout
		out[i]=paste(tout,suffix[i],sep="")
	}
	return(out)
}

