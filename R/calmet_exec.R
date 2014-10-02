#' Run the CALMET model
#' @description Run the CALMET model.
#' @param calmet_exec the path and name of the CALMET executable.
#' @export calmet_exec

calmet_exec <- function(calmet_exec){
  
  # Obtain list of CALMET input files
  calmet_in_files <- list.files(pattern = "^calmet_in.*.txt")
