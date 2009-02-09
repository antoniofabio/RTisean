#Checks whenever the TISEAN path is set. If not, a path is interactively asked
.loadPaths <- function() {
  settingsFile <- ".RTiseanSettings"
  usrdir <- Sys.getenv("HOME")
  settingsPath <- file.path(usrdir, settingsFile)
  if(settingsFile %in% dir(usrdir, all=TRUE)) { #file found
    path <- gsub("(.*) *$","\\1",readLines(settingsPath)[1])
    .checkPath(path)
    assign(".TISEANpath",path,env=.GlobalEnv)
    return()
  }
  path <- setTISEANpath()
  assign(".TISEANpath",path,env=.GlobalEnv)
}

.checkPath <- function(path) {
  if(.Platform$OS.type=="windows")
    binlist <- paste(.binlist,".exe",sep="")
  else
    binlist <- .binlist
  count <- sum(binlist %in% dir(path))
  if(count==0)
    stop("no TISEAN executables found in that directory. Please set a proper TISEAN executables path using 'setTISEANpath'")	
  return()
}

setTISEANpath <- function(path, GUI=interactive()) {
  if(interactive() && missing(path)) {
    if(GUI && require(tcltk))
      path <- tclvalue(tkchooseDirectory(title="Please select TISEAN executables directory"))
    else
      path <- readline("TISEAN executables directory: ")
    .checkPath(path)
  }
  settingsPath <- file.path(Sys.getenv("HOME"), ".RTiseanSettings")
  writeLines(path, settingsPath)
  return(path)
}

setTISEANdocs <- function(path, GUI=interactive()) {
  if(missing(path) && GUI && require(tcltk))
    path <- as.character(tkchooseDirectory(title="Please select TISEAN executables directory"))
  assign(".TISEANdocs",path,env=.GlobalEnv)
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
    stop("can't find ",routine, " help")
  browseURL(rfile)
}

.getTempFName <- function()
  gsub("\\\\","/",tempfile())

#routinename: character vector containing routine name
#input: input object, to be serialized before passed to the routine
#...: named list of routine options (excluding filenames)
#suffixes: optional char vector of suffixes for each output file produced by 'routinename'
#noout: if true, the routine output is explicitely redirected to a file
#parobjects: optional named list of further objects to be passed as input files parameters
#remove.extras: optionally remove extra files
callTISEAN <- function(routinename, input, ..., suffixes=NULL, noout=FALSE, parobjects=NULL, 
                       remove.extras=FALSE) {
  oldOpts <- options(scipen=10)
  routinename <- paste(routinename, ifelse(.Platform$OS.type=="windows",".exe",""),sep="")
  if(!exists(".TISEANpath"))
    .loadPaths()
  routinename <- shQuote(file.path(.TISEANpath, routinename))
  opts <- .listToOpts(list(...))
  opts <- paste(opts, "-V0")
  if(!missing(input))
    .serialize(input, tin <- .getTempFName())
  else
    tin <- ""
  tout <- .getTempFName()

  if(!is.null(parobjects)) { #add further command line options
    nms <- names(parobjects)
    parfilenames <- list()
    for(nm in nms) {
      .serialize(parobjects[[nm]], parfilenames[[nm]] <- .getTempFName())
      opts <- paste(opts, " -", nm, parfilenames[[nm]],sep="")
    }
  }
  cmd <- paste(routinename," ",tin, " ",opts,sep="")
  if(!noout) {
    cmd <-  paste(cmd, " -o",tout, sep="")
    try(system(cmd, intern = FALSE))
  }	else
  write(system(cmd, intern = TRUE), file = tout)
  ans <- list()
  if(!is.null(suffixes))
    for(sf in suffixes)
      ans[[sf]] <- TISEANoutput(paste(tout,sf,sep=""))
  else
    ans <- TISEANoutput(tout)
  file.remove(tin, paste(tout, suffixes, sep=""))
  if(!is.null(parobjects)) {
    file.remove(unlist(parfilenames))
    lapply(parfilenames, function(x) {
      file.remove(x)
      if(remove.extras)
        file.remove(dir(pattern=paste(x,"*",sep="")))
    })
  }
  options(oldOpts)
  return(ans)
}

#Converts (named) argument list to TISEAN-style command line options
.listToOpts <- function(lst) {
  if(length(lst)==0) 
    return()
  ans <- ""
  nms <- names(lst)
  for(i in 1:length(lst)) {
    if(length( lst[[i]] )==1)
      ans <- paste(ans, " -",nms[i],lst[[i]],sep="")
    else {
      tmp <- paste(lst[[i]], collapse=",")
      ans <- paste(ans, " -",nms[i], tmp , sep="")
    }
  }
  return(ans)
}

#Call the selected routine without any arguments translation. 
#	Output has to be extracted manually from intermediate files
callNativeTISEAN <- function(routinename, opts) {
  routinename <- paste(routinename, ifelse(.Platform$OS.type=="windows",".exe",""),sep="")
  if(!exists(".TISEANpath"))
    .loadPaths()
  routinename <- paste("\"", file.path(.TISEANpath, routinename),"\"",sep="")
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
  rownames(ans) <- NULL
  file.remove(tmp)
  ans
}

print.TISEANblock <- function(x, ...)
  cat(x, sep="\n")

#Should return a list of 'TISEANblock's
as.list.TISEANoutput <- function(x, ...) {
  lns <- attr(x,"txt")
  isComment <- function(x)
    any(grep("^ *#|^ *$", x)) #catch blank lines and lines starting with '#'
      ans <- list()
  cbl <- character()
  now <- NULL
  current <- NULL
  for(cln in lns) {
    now <- ifelse(isComment(cln), "head","non-head")
    if(is.null(current))
      current <- now
    if(now==current) #add current line to current block
      cbl <- c(cbl,cln) 
    else { #type of block change: store current block, start a new one with current line
      tmpblck <- ifelse(current=="head",
                        list(cbl),
                        list(structure(cbl, class="TISEANblock"))
			)
      ans <- c(ans, tmpblck)
      cbl <- cln
    }
    current <- now
  }
                                        #store last block
  tmpblck <- ifelse(current=="head",
                    list(cbl),
                    list(structure(cbl, class="TISEANblock")))
  ans <- c(ans, tmpblck)
                                        #remove spurious blank lines
  for(i in which(sapply(ans, function(x) !inherits(x,"TISEANblock")))) {
    tmp <- ans[[i]][ans[[i]]!=""] #remove empty lines
    ans[[i]] <- if(length(tmp)>0) tmp else "" #but there must be at least one
  }
  return(ans)
}

as.matrixList <- function(x) {
  is <- which(sapply(x, inherits, "TISEANblock"))
  x[is] <- lapply(x[is],as.matrix)
  return(x)
}

#Store object 'x' in file named 'filename'
.serialize <- function(x, filename, ...)
  UseMethod(".serialize")

.serialize.default <- function(x, filename, ...) {
  if(!is.null(attr(x,"txt")))
    writeLines(x, filename)
  else
    write(x, filename, ncolumns=NCOL(x))
}

#Serialize list elements, optionally separating them by blank lines
.serialize.list <- function(x, filename, sep=TRUE, ...) {
  con <- file(filename, "w")
  lapply(x, 
         function(bit, filename) {
           .serialize(bit, filename)
           if(sep) writeLines("",filename)
         }, filename=con)
  close(con)
}

#Serialize a typical TISEAN list of matrices separated by heads
.serialize.TISEANmatrixlist <- function(x, filename, ...) {
  if(!inherits(x,"list"))
    stop("non-list argument")
  con <- file(filename, "w")
  nl <- floor(length(x)/2)
  for(i in 1:nl) {
    .serialize(x[[(i-1)*2+1]], con)
    .serialize(x[[2*i]], con)
    cat("\n",file=con)
  }
  close(con)	
}

.serialize.character <- function(x, filename, ...) 
  write(x, filename)

.serialize.matrix <- function(x, filename, ...)
  write(t(x), filename, ncolumns=ncol(x))

.serialize.TISEANoutput <- function(x, filename, ...)
  writeLines(attr(x,"txt"), filename)

.serialize.TISEANblock <- function(x, filename, ...)
  writeLines(x, filename)

.serialize.character <- function(x, filename, ...)
  writeLines(x, filename)

.cleanList <- function(x, pattern="^#.*m= +(.*)$", substitute="dim\\1") {
  n <- floor(length(x)/2)
  nms <- unlist(x[(1:n-1)*2+1])
  x <- x[(1:n)*2]
  names(x) <- gsub(pattern, substitute, nms)
  return(x)
}
