checkcolumnsinput <- function(a,c,m,dimm){

#checks if c is a vector and m is a vector of length dimm in case it is not -1
#checks that if m!=-1, its entries are positive
#in case c is not -1 and m is not -1: returns checkmulti(a,c,m[1])
#in case c==-1 and m!=-1 returns checkcolumns(a,m[1])
#in case c!=-1 and m==-1 returns checkcolumns(a,c)
# TRUE if c==-1 and m==-1

if (!is_vector(c) | !is_vector(m)){
	return(FALSE)
}

if (length(m)!=dimm & !(length(m)==1 & m[1]==-1)){
	return(FALSE)
}

if (length(c)==1 & c[1]==-1 & length(m)==1 & m[1]==-1){
	return(TRUE)
}

if (!(length(m)==1 & m[1]==-1)){
	for (i in 1:dimm){
		if (!checkpositive(m[i])){
			return(FALSE)
		}
	}	
}

if (length(c)==1 & c[1]==-1){
#if arrived here, m is not -1 (and length(m)=dimm)
	return(checkcolumns(a,m[1]))
}

if (length(m)==1 & m[1]==-1){
#if arrived here, c is not -1
	return(checkcolumns(a,c))
}

return(checkmulti(a,c,m[1])) #in the case that both c and m are not -1

}

