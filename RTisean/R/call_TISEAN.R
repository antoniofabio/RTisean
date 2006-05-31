#This calls directly a TISEAN executable, writing input and outputs to tempfiles
#Result is returned as a list of char vectors, one per output files
call_TISEAN_exec <- function(name, opts, inLines) {
	tin <- tempfile("tin")
	writeLines(inLines,tin)
	tout <- tempfile("tout")
	opts <- paste(tin, " ", opts, "-o",tout," -V0 ",sep="")
	cmd <- paste(file.path(.path.package("RTisean", quiet = FALSE), "exec", name), 
		ifelse(.Platform$OS.type=="windows", ".exe", "")," ", opts, sep="")
#	cat(paste("Executing command:\n",cmd,"\n"))
	res <- try(system(cmd))
	ans <- readLines(tout)
	file.remove(tin,tout)
	return(ans)
}

print.nativeTisean <- function(x, ...)
	cat(attr(x,"txt"),sep="\n")

as.matrix.nativeTisean <- function(x) {
	writeLines(attr(x,"txt"),tmp <- tempfile("tmp"))
	ans <- read.table(tmp)
	file.remove(tmp)
	as.matrix(ans)
}

as.list.nativeTisean <- function(x, ...)
	stop("not yet implemented")

as.character.nativeTisean <- function(x)
	attr(x,"txt")
