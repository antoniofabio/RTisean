write_to_file <- function(t,filename){

if (is_nice_list(t)){#list of arrays and vectors of strings
flag=0;
for ( i in 1:length(t)){

	entry=t[[i]]
	if (mode(entry)=="character"){
		for (j in 1:length(entry)){
			.C("write_string_to_file",as.character(entry[j]),as.integer(flag),as.character(filename),PACKAGE="RTisean")
			if (flag==0){
				flag=1
			}
		}
	}
	else{ #we assume that t[[i]] is a matrix array
		d=dim(entry)
		dim(entry)=c(d[1]*d[2])
		.C("write_array_to_file",entry,length(entry),as.integer(d[1]),as.integer(d[2]),as.integer(flag),as.character(filename),PACKAGE="RTisean")
		if (flag==0){
			flag=1
		}
		#append two new lines
		.C("write_string_to_file",as.character("\n"),as.integer(1),as.character(filename),PACKAGE="RTisean")
		.C("write_string_to_file",as.character("\n"),as.integer(1),as.character(filename),PACKAGE="RTisean")	
	}
}
return(0);
}

if (is_vector(t)){
	.C("write_array_to_file",as.double(t),as.integer(length(t)),as.integer(length(t)),as.integer(1),as.integer(0),as.character(filename),PACKAGE="RTisean")
	return(0)
}

if (is_matrix(t)){
	d=dim(t)
	dim(t)=c(d[1]*d[2])
	.C("write_array_to_file",t,length(t),as.integer(d[1]),as.integer(d[2]),as.integer(0),as.character(filename),PACKAGE="RTisean")
	return(0)
}

#in case of none of the above types
return(1)

}

