c2d <- function(lst,a=-1){

options = ""

if (!is_nice_list(lst)){
    print("wrong input")
    return()
}

if (checkint(a)){
		options = paste(options," -a",i2s(a)," ",sep="")	
}
else{
	if (tneq(a,-1)){
		errormessage("a",a)
		return()
	}
	else{
		print("a has to be specified")
	}
}


#debug
 

out=call_TISEANF_extended(lst,options,"c2d")

compout=length(out)   #C and following lines
  if(compout%%2!=0) 
  {outtmp=list()
    for (i in 2:compout)
    outtmp[[i-1]]=out[[i]]
  
  out=outtmp
  }

out=colnamesout(out,1,c("epsilon","local.slope"))   

out=dimcleanout(out)

return(out)

}
