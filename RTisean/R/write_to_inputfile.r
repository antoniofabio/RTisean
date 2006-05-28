write_to_inputfile <- function(t){
	tin <- input_filename()
	write.table(t,tin, row.names=FALSE, col.names=FALSE)
	return(tin)
}
