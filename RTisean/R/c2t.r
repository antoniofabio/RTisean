c2t <- function(lst){

options = ""

if (!is_nice_list(lst)){
    print("wrong input")
    return()
}

out=call_TISEANF_extended(lst,options,"c2t")

out=colnamesout(out,1,c("epsilon","Takens estimator"))#C and following lines

out=dimcleanout(out)

return(out)

}
