colnamesout <- function(lst,skip,namesvect) {  #assigns the columns of all matrices
                                      #in a list a name, possibly skipping even 
                                      #list elements if "skip"=1
	numelements <- length(lst)
	skip <- skip+1
	for (i in seq(skip,numelements,by=skip))
		colnames(lst[[i]])<-namesvect
	return (lst)
}