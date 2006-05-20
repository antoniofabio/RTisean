is_matrix <- function(a){
	if (mode(a)!="numeric"){
		return(FALSE)
	}
	else{	
		dm=dim(a)
		if (length(dm)==2 && length(a)==dm[1]*dm[2]){
			return(TRUE)
		}
		else{
			return(FALSE)
		}
	}
}

