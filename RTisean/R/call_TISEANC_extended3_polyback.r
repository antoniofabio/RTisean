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
   options=paste(tin,options,"-o",tout)

   out=write_to_inputfile(a)
   if (out==1){
	print("wrong input")
        return()
   }	

   out = .C("call_TISEAN_bare", as.character(options), as.character(function_name),PACKAGE="RTisean")
   
   .C("delete_file",as.character(tin),PACKAGE="RTisean")
   for (i in 1:length(param)){
	.C("delete_file",param_filename(i),PACKAGE="RTisean")
   }
   out=list()
   suffix=polyback_aux()
   for (i in 1:length(suffix)){
   	out[[i]] = read_TISEAN_new(paste(".",suffix[i],sep=""))
   }
   
   return(out)
}
