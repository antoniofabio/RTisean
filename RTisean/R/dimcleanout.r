dimcleanout=function(out) #convert lists containing [[x]]="# dim = y", [[x+1]]=matrix
                          #into lists containing only [[x]]$dimy=matrix
{
namesm=cleanfinalout(out,0)
namesmm=NULL
    for (i in 1:length(namesm))
    namesmm=c(namesmm,chartonum(namesm[[i]],"=",2))

out=cleanfinalout(out,1)
out=prettyout(out,NULL,paste("dim",as.character(namesmm),sep=""))
  
return(out)
  
}