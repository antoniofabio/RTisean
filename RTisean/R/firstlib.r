.First.lib <- function(lib,pkg)
{
   library.dynam("RTisean",pkg,lib)
   .C("set_process_id",as.integer(Sys.getpid()),PACKAGE="RTisean")
   cat("tiseanR 0.1-1 loaded and initialized\n")
}


