polypar <- function(m=2,p=3){
	out <- as.matrix( callTISEAN(routinename="polypar", m=m, p=p) )
	return(out)
}
