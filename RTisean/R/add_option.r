add_option <- function(options,optionname,values){
		
	if (is_vector(values)){

		options=paste(options," -",optionname,sep="")
		for (i in 1:length(values)){
			if (i>1){
				options=paste(options,",",sep="")
			}
			options=paste(options,values[i],sep="")
		}
		options=paste(options," ",sep="")
	}
	return(options) 


}


