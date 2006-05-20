c2g <- function(lst){

options = ""

if (!is_nice_list(lst)){
    print("wrong input")
    return()
}


#debug
 

out=call_TISEANF_extended(lst,options,"c2g")

out=colnamesout(out,1,c("epsilon", "corr.integral", "log.der."))   #C and following lines

out=dimcleanout(out)
  
return(out)

}
