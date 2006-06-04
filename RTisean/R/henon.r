henon <- function(l=-1,A=1.4,B=0.3,X=-1,Y=-1,x=10000) {
	out <- callTISEAN("henon", l=l, A=A, B=B, X=X, Y=Y, x=x)
	return(as.matrix(out))
}
