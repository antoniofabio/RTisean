checkposint <- function(a){

out = .C("get_INT_MAX",im = integer(1),PACKAGE="RTisean")
INT_MAX = out$im

if (mode(a)!="numeric"  | length(a)!=1){
return(FALSE)
}
if (a!=round(a)){
return(FALSE)
}
if (a>INT_MAX){
return(FALSE)
}
if (a<=0){
return(FALSE)
}

return(TRUE)

}
