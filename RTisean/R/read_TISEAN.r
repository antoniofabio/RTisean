#Reads all created output files and converts to proper R objects
read_TISEAN <- function(filenames,split=FALSE) {
	ofn <- filenames
	ans <- list()
	for(i in 1:length(ofn)) {
		cfn <- ofn[i]
		if(!split)
			ansi <- read.table(cfn)
		else {
			lns <- readLines(cfn)
			nr <- length(lns)
			j <- 0; j2 <- numeric()
			nfields <- 0
			ansi <- list()
			ansinames <- character()
			repeat {
				j <- j+1
				cln <- lns[j]
				if(substr(cln,1,1)=="#") {
					nfields <- nfields + 1
					for(j2 in (j+1):(nr-1)) {
						if(substr(lns[j2+1],1,1)=="#")
							break
					}
					ansi[[nfields]] <- convertBlock(lns[(j+1):j2])
					ansinames[nfields] <- cln
					j <- j2
				}
				if(j>=nr) 
					break
			}
			names(ansi) <- ansinames
		}
		ans[[i]] <- ansi
	}
	names(ans) <- ofn
	return(ans)
}

convertBlock <- function(lines) {
	tmpfname <- tempfile()
	writeLines(lines,tmpfname)
	res <- as.matrix(read.table(tmpfname))
	file.remove(tmpfname)
	return(res)
}
