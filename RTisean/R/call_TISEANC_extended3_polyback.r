call_TISEANC_extended3_polyback <- function(a, options, function_name,suffix,param,paramchars) # a is a numeric vector, function_name is the TISEAN function name
{

# adresses the case when there are several parameter files and several output files (distinguished by suffix) 
# paramchars,suffix are arrays of strings
# param is a **list** of array parameters

# write parameters to temporary files
   paramoptions=""
   for (i in 1:length(param)){
	write_to_file(param[[i]],param_filename(i))
      paramoptions=paste(paramoptions," -",paramchars[i]," ",param_filename(i)," ",sep="")
   }	

   options = paste(" ",options," ",paramoptions," -V0 ",sep="")
   tin=input_filename()
   tout=output_filename()
   options=paste(tin,options,"-o",tout,sep="")

   out=write_to_inputfile(a)
	if (out==1)
		stop("wrong input")


   out = .C("call_TISEAN_bare", as.character(options), as.character(function_name),PACKAGE="RTisean")
   
	file.remove(as.character(tin))
   for (i in 1:length(param)){
		file.remove(param_filename(i))
   }
   out=list()
   suffix=polyback_aux()
   for (i in 1:length(suffix)){
   	out[[i]] <- read_TISEAN_new(paste( ifelse(suffix[i]!="", ".", ""), suffix[i], sep=""))
   }
   
   return(out)
}
