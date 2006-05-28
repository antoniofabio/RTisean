call_TISEAN_bare <- function(args, routine) {
	cmd <- paste(file.path(.path.package(package, quiet = FALSE), routine), args)
	res <- try(system(cmd))
	return(res)
}
