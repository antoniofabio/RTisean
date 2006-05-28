call_TISEAN_bare <- function(args, routine) {
	cmd <- paste(file.path(.path.package("RTisean", quiet = FALSE), "exec", routine), args)
	res <- try(system(cmd))
	return(res)
}
