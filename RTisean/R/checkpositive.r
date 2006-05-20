checkpositive <- function(a){

if (mode(a)!="numeric"  | length(a)!=1){
return(FALSE)
}
else{
if (a<=0){
return(FALSE)
}
}

return(TRUE)

}
