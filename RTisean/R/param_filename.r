param_filename <- function(ordinals){
	out=character(length(ordinals))
	for (i in 1:length(ordinals)){
		tparam=.C("Rparam_filename",tparam=character(1),as.integer(ordinals[i]),PACKAGE="RTisean")$tparam
		out[i]=tparam
	}
	return(out)
}

