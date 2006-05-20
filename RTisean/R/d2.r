d2 <- function(series,l=-1,x=0,d=1,M=-1,c=-1,t=0,R=-1,r=-1,scale=100,N=1000,E=FALSE,pretty=FALSE){ #C

#-------------------------  includes multiple components options --------------------------


options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (checkposint(l)){
		options = paste(options," -l",i2s(l)," ",sep="")	
} else{
	if (tneq(l,-1)){
		errormessage("l",l)
		return()
	}
}

if (checkint(x)){
	options = paste(options," -x",i2s(x)," ",sep="")	
}  else{
	errormessage("x",x)
	return()
}

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}   else{
	errormessage("d",d)
	return()
}

if (checkcolumnsinput(series,c,M,2)){
	if (tneq(c,-1)){
		options=add_option(options,"c",c)
	}
	if (tneq(M,-1)){
		options=add_option(options,"M",M)
	} }  else{
	print("wrong input")
	return()
}

if (checkint(t)){
		options = paste(options," -t",i2s(t)," ",sep="")	
}    else{
	errormessage("t",t)
	return()
}

if (checkpositive(R)){
	options = paste(options," -R",d2s(R)," ",sep="")	
}    else{
	if (tneq(R,-1)){
		errormessage("R",R)
		return()
	}
}


if (checkpositive(r)){
	options = paste(options," -r",d2s(r)," ",sep="")	
}    else{ 
	if (tneq(r,-1)){
	      errormessage("r",r)
		return()
	}
}

if (checkposint(scale)){
	options = paste(options, " -#",i2s(scale)," ",sep="")
}  else{
	errormessage("scale",scale)
	return()
}

if (checkint(N)){
	options = paste(options, " -N",i2s(N)," ",sep="")
}  else{
	errormessage("N",N)
	return()
}


if (E==TRUE){
	options = paste(options, " -E"," ",sep="")
}

#debug

outnames=c(".c2",".d2",".h2",".stat") #C

out=call_TISEANC_extended2(series,options,"d2",outnames) #C

exclude=4 #C

out=prettyout(out,exclude,outnames[-exclude])  #C

out[[1]]=colnamesout(out[[1]],1,c("epsilon","correlation sum")) #C

out[[2]]=colnamesout(out[[2]],1,c("epsilon","dimension"))         #C

out[[3]]=colnamesout(out[[3]],1,c("epsilon","entropy"))  #C   and following lines    


  if (pretty)
  {names1=cleanfinalout(out[[1]],0)
  names11=chartonum(names1[[1]][2],"=",2)
    for (i in 2:length(names1)) 
    names11=c(names11,chartonum(names1[[i]],"=",2))
    
  out[[1]]=cleanfinalout(out[[1]],1)
  out[[1]]=prettyout(out[[1]],NULL,paste("dim",as.character(names11),sep=""))
    
  names2=cleanfinalout(out[[2]],0)
  names22=chartonum(names2[[1]][2],"=",2)
    for (i in 2:length(names2)) 
    names22=c(names22,chartonum(names2[[i]],"=",2))
    
  out[[2]]=cleanfinalout(out[[2]],1)
  out[[2]]=prettyout(out[[2]],NULL,paste("dim",as.character(names22),sep=""))  
    
  names3=cleanfinalout(out[[3]],0)
  names33=chartonum(names3[[1]][2],"=",2)
    for (i in 2:length(names3)) 
    names33=c(names33,chartonum(names3[[i]],"=",2))
    
  out[[3]]=cleanfinalout(out[[3]],1)
  out[[3]]=prettyout(out[[3]],NULL,paste("dim",as.character(names33),sep="")) 
  }
  
    
return(out)

}
