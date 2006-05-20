input_filename <- function(){

tin=.C("Rinput_filename",tin=character(1),PACKAGE="RTisean")$tin
return(tin)

}

