call_TISEANF_extended3 <- function(a, bare_options, function_name,suffix,param,paramchars) # a is a numeric vector, function_name is the TISEAN function name
{

# param is assumed to be a list
   paramoptions=""
   for (i in 1:length(param)){
	write_to_file(param[[i]],param_filename(i))
      paramoptions=paste(paramoptions," -",paramchars[i]," ",param_filename(i)," ",sep="")
   }	
   options = paste(" ",bare_options," ",paramoptions," -V0 ",sep="")


    out=write_to_inputfile(a)
    if (out==1){
	print("wrong input")
        return()
    }
    tin=input_filename()
    tout=output_filename()
    options=paste(tin,options," -V0 -o",tout)
    
	toptions=param_filename(0)
	.C("write_string_to_file2",as.character(options),as.integer(0),as.character(toptions),PACKAGE="RTisean")
	.Fortran(function_name,as.character(toptions),PACKAGE="RTisean")
	.C("delete_file",as.character(toptions),PACKAGE="RTisean")

   for (i in 1:length(param)){
	.C("delete_file",param_filename(i),PACKAGE="RTisean")
   }
   .C("delete_file",as.character(tin),PACKAGE="RTisean")
	
   out=list()
   for (i in 1:length(suffix)){
   	out[[i]] = read_TISEAN(suffix[i])
   }
   
   return(out)

}

