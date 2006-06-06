endtoend <- function(series,l,x=0,m,c=1){
	args <- list(routinename="endtoend", input=series, x=x, c=c)
	if(!missing(l))
		args <- c(args, l=l)
	if(!missing(m))
		args <- c(args, m=m)

	out <- as.list(do.call(callTISEAN,args))
	nl <- length(out)/2
	out <- out[0:(nl-1)*2+1]

	ans <- list()
	headRegExp <- "length:(.*) offset:(.*) lost:(.*) %"
	for(i in 1:length(out)) {
		ans[[i]] <- numeric()
		bl <- out[[i]]
		ans[[i]] <- c(
			length = as.numeric(gsub(headRegExp, "\\1", bl[1])),
			offset = as.numeric(gsub(headRegExp, "\\2", bl[1])),
			lost = as.numeric(gsub(headRegExp, "\\3", bl[1])),
			jump = as.numeric(gsub("jump: *([0-9\.]*) %", "\\1", bl[2])),
			slip = as.numeric(gsub("slip: *([0-9\.]*) %", "\\1", bl[3])),
			weighted = as.numeric(gsub("weighted: *([0-9\.]*) %", "\\1", bl[4]))
		)
	}
	return(ans)
}
