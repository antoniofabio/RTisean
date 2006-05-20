tneq <- function(a,b){

#compares two numerical types (either vector or matrix) and returns TRUE if these are not equal
#WARNING: may return TRUE also when a,b are equivalent (in case mode(a)!="numeric")
#in case that mode(a)!=numeric or mode(b)!=numeric, returns TRUE

if (mode(a)!="numeric" | mode(b)!="numeric"){
	return(TRUE)
}

if (is_vector(a)){
	if (!is_vector(b)){
		return(TRUE)		
	}
	else{
		if (length(b)!=length(a)){
			return(TRUE)
		}
		else{
			for (i in 1:length(a)){
				if (a[i]!=b[i]){
					return(TRUE)
				}
			}
			return(FALSE) # the two vectors are identical
		}
	}
}

if (is_vector(b)){
	return(TRUE) # if arrived here, a is not a vector
}

if (is_matrix(a) & is_matrix(b)){
	da=dim(a)
	db=dim(b)
	if (da[1]==db[1] & da[2]==db[2]){
		for (i in 1:da[1]){
			for (j in 1:da[2]){
				if (a[i,j]!=b[i,j]){
					return(TRUE)
				}
			}
		}
		return(FALSE) # the two vectors are identical		
	}
	else{
		return(TRUE) # dimensions are 
	}
}
else{	
	return(TRUE) # if one of a,b has "numeric" mode but is not a vector or a matrix
}



}

