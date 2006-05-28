checkint <- function(a){
	if (mode(a)!="numeric"  | length(a)!=1)
		return(FALSE)
	if (a!=round(a))
		return(FALSE)
	return(TRUE)
}
