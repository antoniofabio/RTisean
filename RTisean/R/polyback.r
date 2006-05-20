polyback <- function(series,l=-1,x=0,c=1,m=2,d=1,n=-1,s=1,scale=1,p=-1){

options = ""

if (mode(series)!="numeric"){
	print("wrong input")
	return()
}

if (checkposint(l)){
		options = paste(options," -l",i2s(l)," ",sep="")	
}
else{
	if (tneq(l,-1)){
		errormessage("l",l)
		return()
	}
}

if (checkint(x)){
	options = paste(options," -x",i2s(x)," ",sep="")	
}
else{
	errormessage("x",x)
	return()
}

if (checkcolumns(series,c,1)){
	options=paste(options," -c",c," ",sep="")
}
else{
	errormessage("c",c)
	return()
}


if (checkposint(m)){
	options = paste(options," -m",i2s(m)," ",sep="")	
} 
else{
	errormessage("m",m)
	return()
}

if (checkposint(d)){
	options = paste(options," -d",i2s(d)," ",sep="")	
}
else{
	errormessage("d",d)
	return()
}

if (checkposint(n)){
		options = paste(options," -n",i2s(n)," ",sep="")	
}
else{
	if (tneq(n,-1)){
		errormessage("n",n)
		return()
	}
}


if (checkposint(s)){
	options = paste(options, " -s",i2s(s)," ",sep="")
}
else{
	errormessage("s",s)
	return()
}

if (checkposint(scale)){
	options = paste(options, " -#",i2s(scale)," ",sep="")
}
else{
	errormessage("scale",scale)
	return()
}


if (!is_matrix(p)){
	print("wrong input")
        return()
}
else{
    if (dim(p)[2]!=2 | tneq(p,round(p)) ){
        print("wrong input")
        return()
    }
}

param=list()
param[[1]]=p
out=call_TISEANC_extended3_polyback(series,options,"polyback","",param,"p")


out=prettyout(out,(2:length(out)),NULL)

out1=out[[1]][[1]]
out2=(out[[1]][[2]])
numcolsout2=ncol(out2)
parcoord=NULL

  for (i in 1: nrow(out2))
  parcoord[i]=paste( out2[i,(4:numcolsout2)],collapse="")

out=as.data.frame(rbind( c(out1,"none"),cbind(out2[,1:3],parcoord)))

out$V1=as.numeric(as.vector(out$V1))
out$V2=as.numeric(as.vector(out$V2))
out$V3=as.numeric(as.vector(out$V3))

colnames(out)=c("rem.par","in.err","out.err","removed")

return(out)

}
