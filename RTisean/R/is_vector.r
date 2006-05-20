is_vector <- function(a){
	if (mode(a)!="numeric"){
		return(FALSE)
	}
	else{	
		dm=dim(a)
		if (length(dm)==0 && length(a)>0 ){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
}

