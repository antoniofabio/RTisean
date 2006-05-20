read_TISEAN_new <- function(suffix) # a is a numeric vector, function_name is the TISEAN function name
{
   MAX_BLOCKS = 100	
   
   out = .C("output_format_extended2_new",s = integer(MAX_BLOCKS),e = integer(MAX_BLOCKS),t = integer(MAX_BLOCKS),c=integer(1),as.integer(MAX_BLOCKS),suffix,PACKAGE="RTisean")

   e=out$e
   s=out$s
   t=out$t
   c=out$c



   out <- .Call("read_TISEAN_output_extended2", s, e, t, c, suffix,PACKAGE="RTisean");

   if (c>0){
   	for (i in 1:c){
		if (t[i]!=0){
			dim(out[[i]]) = c(e[i]-s[i]+1,t[i])
		}
   	}

	if (c==1){
		out=out[[1]]
	}
   

   	return(out)
   }
   else{
	print("no result returned")
        return()
   }

}

