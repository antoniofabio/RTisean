xzero <- function(series,l=-1,x=0,c=-1,m=3,d=1,n=-1,k=30,r=-1,f=1.2,s=1){

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

if (c==-1){
	c=c(1,2)
}
if (checkcolumns(series,c,2)){
	options=add_option(options,"c",c)
}
else{
	print("wrong input")
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
	options=paste(options," -n",i2s(n)," ",sep="")
}
else{
	if (tneq(n,-1)){
		errormessage("n",n)
		return()
	}
}

if (checkposint(k)){
	options=paste(options," -k",i2s(k)," ",sep="")
}
else{
		errormessage("k",k)
		return()
}


if (checkpositive(r)){
	options=paste(options," -r",d2s(r)," ",sep="")
}
else{
	if (tneq(r,-1)){
		errormessage("r",r)
		return()
	}
}

if (checkpositive(f)){
	options=paste(options," -f",d2s(f)," ",sep="")
}
else{
		errormessage("f",f)
		return()
}

if (checkposint(s)){
	options=paste(options," -s",i2s(s)," ",sep="")
}
else{
		errormessage("s",s)
		return()
}

#debug
 

out=call_TISEANC_extended(series,options,"xzero")

colnames(out)=c("step","rel.error") #C

return(out)

}
