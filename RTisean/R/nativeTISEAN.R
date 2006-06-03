setTISEANpath <- function(path, GUI=interactive()) {
	if(missing(path) && GUI && require(tcltk))
		path <- as.character(tkchooseDirectory(title="Please select TISEAN executables directory"))
	assign(".TISEANpath",path,env=.GlobalEnv)
	#FIXME: write on disk
}

setTISEANdocs <- function(path, GUI=interactive()) {
	if(missing(path) && GUI && require(tcltk))
		path <- as.character(tkchooseDirectory(title="Please select TISEAN executables directory"))
	assign(".TISEANdocs",path,env=.GlobalEnv)
	#FIXME: write on disk
}

helpTISEAN <- function(routine) {
	if(!exists(".TISEANdocs")) {
		if(interactive() && require(tcltk))
			 assign(".TISEANdocs", as.character(tkchooseDirectory(
				title="Please select TISEAN html docs directory")),env=.GlobalEnv)
		else stop("you first have to set path to TISEAN docs using 'setTISEANdocs(path)'")
	}
	rfile <- paste(routine,"html",sep=".")
	if(rfile %in% dir(file.path(.TISEANdocs, "docs_c")))
		rfile <- file.path(.TISEANdocs,"docs_c",rfile)
	else if(rfile %in% dir(file.path(.TISEANdocs, "docs_f")))
		rfile <- file.path(.TISEANdocs,"docs_f",rfile)
	else 
		stop("can't find ",routine, "help")
	browseURL(rfile)
}

#routinename: character vector containing routine name
#input: input object, to be serialized before passed to the routine
#...: named list of routine options (excluding filenames)
#suffixes: optional char vector of suffixes for each output file produced by 'routinename'
callTISEAN <- function(routinename, input, ..., suffixes=NULL) {
	opts <- .listToOpts(list(...))
	if(!missing(input))
		.serialize(input, tin <- gsub("\\\\","/",tempfile()))
	else
		tin <- ""
	tout <- gsub("\\\\","/",tempfile())
	routinename <- paste(routinename, ifelse(.Platform$OS.type=="windows",".exe",""),sep="")
	if(exists(".TISEANpath"))
		routinename <- file.path(.TISEANpath, routinename)
	cmd <- paste(routinename," ",tin, " ",opts, " -o",tout,sep="")
	try(system(cmd, intern = FALSE))
	ans <- list()
	if(!is.null(suffixes))
		for(sf in suffixes)
			ans[[sf]] <- TISEANoutput(paste(tout,sf,sep=""))
	else
		ans <- TISEANoutput(tout)
	file.remove(tin,paste(tout,suffixes))
	return(ans)
}

.listToOpts <- function(lst) {
	if(length(lst)==0) 
		return()
	ans <- ""
	nms <- names(lst)
	for(i in 1:length(lst))
		ans <- paste(ans, " -",nms[i],lst[[i]],sep="")
	return(ans)
}

#Call the selected routine without any arguments translation. 
#	Output has to be extracted manually from intermediate files
callNativeTISEAN <- function(routine, opts) {
	if(exists(".TISEANpath"))
		routinename <- file.path(.TISEANpath, routinename)
	cmd <- paste(routinename," ",opts, sep="")
	try(system(cmd, intern = FALSE))
}

#Reads lines in file 'fname' and returns an object of class "TISEANoutput", with file lines
#	attached as 'txt' attribute
TISEANoutput <- function(fname, ...)
	structure(list(), txt=readLines(fname), class="TISEANoutput")

as.character.TISEANoutput <- function(x, ...)
	attr(x,"txt")

print.TISEANoutput <- function(x, ...)
	cat(attr(x,"txt"), sep="\n")

as.matrix.TISEANoutput <- function(x, ...)
	as.matrix.TISEANblock(x, ...)

#A vector of character, representing subsequent 'homogenous' lines
TISEANblock <- function(lns, ...)
	structure(lns,class=c("TISEANblock","character"))

as.matrix.TISEANblock <- function(x, ...) {
	.serialize(x, tmp <- tempfile())
	ans <- as.matrix(read.table(tmp,fill=TRUE))
	file.remove(tmp)
	ans
}

print.TISEANblock <- function(x, ...)
	cat(x, sep="\n")

#Should return a list of 'TISEANblock's
as.list.TISEANoutput <- function(x, ...) {
	lns <- attr(x,"txt")
	lns <- lns[lns!=""] #trash out blank lines
	isComment <- function(x)
		return(substr(x,1,1)=="#")
	cmi <- which(isComment(lns)) #Which lines are comments?
	if(length(cmi)==0)
		return(list(TISEANblock(lns)))
	cml <- list()
	i <- 1; j <- 1
	repeat {
		if(i==1)
			cml[[j]] <- numeric()
		if ((i>1)&&((cmi[i]-cmi[i-1])>1)) {
			j <- j+1
			cml[[j]] <- cmi[i]
		} else
			cml[[j]] <- c(cml[[j]],cmi[i])
		i <- i + 1
		if(i>length(cmi))
			break
	}
	nblocks <- length(cml)
	ans <- list()
	for(j in 1:nblocks) {
		ans[[(j-1)*2+1]] <- lns[cml[[j]]]
		if(cml[[j]][length(cml[[j]])]==length(lns))
			break
		rng <- c(max(cml[[j]])+1,ifelse(j<nblocks,min(cml[[j+1]])-1, length(lns)))
		ans[[j*2]] <- TISEANblock(lns[seq(rng[1],rng[2])])
	}
	return(ans)
}

#Store object 'x' in file named 'filename'
.serialize <- function(x, filename, ...)
	UseMethod(".serialize")

.serialize.default <- function(x, filename, ...)
	write(x, filename)

.serialize.matrix <- function(x, filename)
	write(t(x), filename)

.serialize.TISEANoutput <- function(x, filename, ...)
	writeLines(attr(x,"txt"), filename)

.serialize.TISEANblock <- function(x, filename, ...)
	writeLines(x, filename)

.serialize.character <- function(x, filename, ...)
	writeLines(x, filename)
