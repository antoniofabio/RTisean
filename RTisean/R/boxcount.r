boxcount <- function(series,l=-1,x=0,c=-1,d=1,M=-1,Q=2.0,R=-1,r=-1,scale=20){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (checkposint(l)){
		options = paste(options," -l",i2s(l)," ",sep="")	
}  else{
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

if (checkcolumnsinput(series,c,M,2)){
	if (tneq(c,-1)){
		options=add_option(options,"c",c)
	}
	if (tneq(M,-1)){
		options=add_option(options,"M",M)
	}
}     else{
	print("wrong input")
	return()
}	

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}  else{
	errormessage("d",d)
	return()
}

if (checkpositive(Q)){
	options = paste(options," -Q",d2s(Q)," ",sep="")	
}    else{
	errormessage("Q",Q)
	return()
}

if (checkpositive(r)){
	options = paste(options," -r",d2s(r)," ",sep="")	
} else{ 
	if (tneq(r,-1)){
	      errormessage("r",r)
		return()
	}
}

if (checkpositive(R)){
	options = paste(options," -R",d2s(R)," ",sep="")	
} else{ 
	if (tneq(R,-1)){
	      errormessage("R",R)
		return()
	}
}

if (checkposint(scale)){
	options = paste(options, " -#",scale," ",sep="")
} else{
	errormessage("scale",scale)
	return()
}


#debug
 

outtmp=call_TISEANC_extended(series,options,"boxcount")

out=cleanfinalout(outtmp,1)     #C and all following lines

out=colnamesout(out,0,c("epsilon","entropy","diff. entropy"))

outlst=list()

  if(length(M)==1)  M=c(1,10)

 if (M[1]==1)  {
  outlst[[paste("comp",1,sep="")]]=list()   
     for (j in 1:M[2]) 
     outlst[[paste("comp",1,sep="")]][[paste("embdim",j,sep="")]]=out[[j]]
 } else {

   for (i in 1:M[1]){
   cnt=1 
   outlst[[paste("comp",i,sep="")]]=list()   
      for (j in seq(i,length(out),by=(M[2]-1))) {
      outlst[[paste("comp",i,sep="")]][[paste("embdim",cnt,sep="")]]=out[[j]]
      cnt=cnt+1
      }
   }
 }  
  
#components=M[1]  
#embeddims=M[2]   
    
#outnames1=rep(1:components,embeddims)
#outnames=outnames2=NULL

 # for (i in 1:embeddims)
  #outnames2=c(outnames2,rep(i,components))

 # for (i in 1:(components*embeddims))
 # outnames[i]=paste("Comp",outnames1[i],"EmbedDim",outnames2[i],sep="")

  #out=prettyout(out,NULL,outnames)
  #out=colnamesout(out,0,c("epsilon","entropy","diff. entropy"))

  out=outlst
  
return(out)

}

