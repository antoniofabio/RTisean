chartonum=function(lstcomp,splitchar,subcomp) {#takes a component of a list made up of messy
                                              #character strings and cleans it to extract a vector
                                              #of numbers. splitchar is tipically " " or "=", subcomp
                                              #is the element of the vector, resulting after the
                                              #first splitting, that contains the number

strings=strsplit(lstcomp,split=splitchar)

cleanstrings=NULL

  for (i in 1:length(strings))
  cleanstrings[i]=as.numeric(unlist(strsplit(strings[[i]][subcomp],split="\n")))



return(cleanstrings)
}
  

