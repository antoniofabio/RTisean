polyback_aux <- function(){
  #used with polyback, renames param_filename(1).* files
  #into output_filename(1).*, and returns the list of suffixes
  #of the latter
  
  name=param_filename(1)
  suffixes=find_suffixes(name)
  for (s in suffixes){
    system(paste("mv ",param_filename(1),".",s," ",output_filename(),".",s,sep=""),invisible=TRUE) 
    #should work on windows and linux 
  }
  suffixes=find_suffixes(output_filename())
  return(suffixes)

}

