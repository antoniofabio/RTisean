cleanfinalout=function(lst,skip) {    #removes (tipically character strings)
                                      #components skip,skip+2,...
                                      #of a list that contains data that
                                      #are not processable by any of
                                      #the tisean routines
cleanlst=list()
numelements=length(lst)
skip=skip+1
cnt=1

  for (i in seq(skip,numelements,by=2))
  {
  cleanlst[[cnt]]=lst[[i]]
  cnt=cnt+1
  }
return (cleanlst)

}