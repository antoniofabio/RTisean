errormessage<- function(name,value,withvalue=FALSE){
		if (withvalue==TRUE)
			stop(paste(name,"value of",value,"is not allowed"))
		else
			stop(paste("this value of",name,"is not allowed"))
}

