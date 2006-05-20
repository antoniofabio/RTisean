checkmulti <- function(a,c=-1,m=-1){

#this makes the following checks
#a must be a vector or a matrix, c must be a vector, m must be a vector of length 1
#if c==-1, m must be positive and not greater than the number of columns of a.
#c and m cannot be both -1
#if m==-1, the column numbers given by c must be all distinct and not greater than the number of columns of a.
#if m!=-1 and c!=-1, c must have length 1, and m must be a positive number not greater than the number of columns of a
#   minus c plus 1. 

if ((!is_vector(a) & !is_matrix(a)) | !is_vector(c) | !is_vector(m)){
	return(FALSE)
}

if (length(m)>1 | (length(c)==1 & m[1]==-1 & c[1]==-1)){
	return(FALSE)
}

if (m[1]==-1){
	return(checkcolumns(a,c))
}

# at this point, both m and c are not -1

if (length(c)!=1){
	return(FALSE)
}

if (!checkposint(m[1]) | !checkposint(c[1])){
	return(FALSE)
}

if (is_vector(a)){
	if (c[1]!=1 | m[1]!=1){
		return(FALSE)
	}
	return(TRUE)
}

if (is_matrix(a)){
	d=dim(a)[2]
	if (m[1]>d){
		return(FALSE)
	}
	if (c[1]<1 | c[1]>d){
		return(FALSE)
	}
	if (d-c[1]+1<m[1]){
		return(FALSE)
	} 
	return(TRUE)
}

return("this point in checkmulti.r must not be reached") 

}

