find_suffixes <- function(filename){
  #returns the list of suffixes
  dirlist=dir()
  suffixes=c()
  
  for (e in dirlist){
    s=strsplit(e,".",fixed=TRUE)
    if (s[[1]][1]==filename){
        if (length(s[[1]])>1){
          suffixes[length(suffixes)+1]=s[[1]][length(s[[1]])]
        }
        else{
          suffixes[length(suffixes)+1]=""
        }
    }
  } 
  return(suffixes)

}

