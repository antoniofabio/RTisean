write_to_inputfile <- function(t){

	tin=.C("Rinput_filename",tin=character(1),PACKAGE="RTisean")$tin
	out=write_to_file(t,tin)
	return(out)
}
