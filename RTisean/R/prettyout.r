prettyout = function(lst,exclude,outnames) {  #cleans a list, possibly removing some
                                              #elements from it and assigns names to each
                                              #[[x]] component (useful when Tisean's
                                              #output is made up of more files 
                                              #input: list to clean, element to get
                                              # rid of, names to assign

originalength=length(lst)

newout=list()

lstelements=1:originalength

  if ( length(exclude)>0) lstelements=lstelements[-exclude]

cnt=1

  for (i in lstelements)
  {newout[[cnt]]=lst[[i]]
   cnt=cnt+1
  }

names(newout)=outnames

return (newout)

}