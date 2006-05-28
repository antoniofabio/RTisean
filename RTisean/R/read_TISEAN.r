#Reads all created output files and converts to proper R objects
read_TISEAN <- function(filenames, suffix) {
	ofn <- filenames
	ans <- list()
	for(i in 1:length(ofn)) {
		cfn <- ofn[i]
		ans[[i]] <- read.table(cfn)
	}

	return(ans)
}
