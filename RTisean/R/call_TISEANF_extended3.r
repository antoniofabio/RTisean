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
    if (out==1)
			stop("wrong input")

    tin=input_filename()
    tout=output_filename()
    options=paste(tin," ",options," -V0 -o",tout, sep="")
    
	toptions=param_filename(0)
	.C("write_string_to_file2",as.character(options),as.integer(0),as.character(toptions),PACKAGE="RTisean")
	.Fortran(function_name,as.character(toptions),PACKAGE="RTisean")
	file.remove(toptions)

   for (i in 1:length(param)){
		file.remove(param_filename(i))
   }
	file.remove(tin)
	
   out=list()
   for (i in 1:length(suffix)){
   	out[[i]] = read_TISEAN(suffix[i])
   }
   
   return(out)

}

