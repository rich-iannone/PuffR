#' Set one or more CALPUFF area source parameters
#' @description This function validates and writes CALPUFF area source parameters.
#' @param calpuff_inp 
#' @param nar1 
#' @param iaru 
#' @param nsar1 
#' @param nar2 
#' @param arsource_params 
#' @param arsource_coords 
#' @export calpuff_14_area_source_params

calpuff_14_area_source_params <- function(calpuff_inp = "calpuff_template.txt",
                                          nar1 = NULL,
                                          iaru = NULL,
                                          nsar1 = NULL,
                                          nar2 = NULL,
                                          arsource_params = NULL,
                                          arsource_coords = NULL){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("NAR1", "IARU", "NSAR1", "NAR2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(nar1, iaru, nsar1, nar2)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
