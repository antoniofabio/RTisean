is_nice_list <- function(lst){              #return false if list is nice?
	
	if (mode(lst)!="list"){
	return(FALSE)
	}

	if (length(lst)<1 || round(length(lst)/2)!=length(lst)/2){    #se dispari
	return(FALSE)
	}

	for (i in 1:(length(lst)/2) ){       
	if (length(lst[[i*2-1]])<1){      #se non c'è scritto niente dove dovrebbe esserci la stringa
		return(FALSE)
	}	

	if (mode(lst[[i*2-1]])!="character"){ #è previsto che i termini dispari abbiano una stringa
		return(FALSE)
	}
	
	td=dim( lst[[2*i]] ) 	   #dimensioni della matrice (devono essere due valori)
	
	if (length(td)<2){
		return(FALSE)
	}


	if (td[2]!=2){ #se non due colonne
		return(FALSE)
	}
	}

	return(TRUE)

}

