polyback_aux <- function(){
  #used with polyback, renames param_filename(1).* files
  #into output_filename(1).*, and returns the list of suffixes
  #of the latter
  
  name <- param_filename(1)
  suffixes <- find_suffixes(name)
  for (s in suffixes){
		file.rename(paste(param_filename(1),".",s,sep=""),paste(output_filename(),".",s,sep=""))
    #should work on windows and linux 
  }
  suffixes <- find_suffixes(output_filename())
  return(suffixes)
}

