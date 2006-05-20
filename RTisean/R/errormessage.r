errormessage<- function(name,value,withvalue=FALSE){
		if (withvalue==TRUE){
			print(paste(name,"value of",value,"is not allowed"))
		}
		else{
			print(paste("this value of",name,"is not allowed"))
		}
}

