checkcolumns <- function(a,c,len=-1){


# controls that c is a vector and a is either a vector or a matrix.
# controls that the entries of c are all distinct and within range.
# if len is specified and not -1, controls that the length of c is equal to len.

if (!is_vector(c) | (!is_vector(a) & !is_matrix(a)) | !is_vector(len)){
	return(FALSE)
}

if (length(len)>1 | (len[1]!=-1 & length(c)!=len[1])){
	return(FALSE)
} 

	d=1
	if (is_matrix(a)){
		d=dim(a)[2]
	}
	
	if (length(c)>d){
		return(FALSE)
	}

	temp=c(1:d)

	for (i in 1:d){
		temp[i]=0
	}
	
	for (i in 1:length(c)){
		if (c[i]<1 | c[i]>d){
			return(FALSE)
		}
		temp[c[i]]=temp[c[i]]+1
	}

	for (i in 1:d){
		if (temp[i]>1){
			return(FALSE)
		}
	}

	return(TRUE)

}

