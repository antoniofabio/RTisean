call_TISEANC_extended <- function(a, options, function_name) # a is a numeric vector, function_name is the TISEAN function name
{
# addresses the case when there is only one input file and only one output file

   options = paste(" ",options," -V0 ",sep="")
   tin=input_filename()
   tout=output_filename()
   options=paste(tin,options,"-o",tout)

   out=write_to_inputfile(a)
   if (out==1){
        print("wrong input")
	return()
   }	

   out = .C("call_TISEAN_bare", as.character(options), as.character(function_name),PACKAGE="RTisean")
   
   out = read_TISEAN("")
   .C("delete_file",as.character(tin),PACKAGE="RTisean")
   return(out)

}

