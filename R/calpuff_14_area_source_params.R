#' Set one or more CALPUFF area source parameters
#' @description This function validates and writes CALPUFF area source parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param nar1 the number of area sources.
#' @param iaru the units used for line source emissions, where the possible selections are: (1) "g/m^2/s", (2) "kg/m^2/hr", (3) "lb/m^2/hr", (4) "tons/m^2/yr", (5) "Odour Unit * m/s", (6) "Odour Unit * m/min", and (7) "metric tons/m^2/yr".
#' @param nsar1 the number of source-species combinations with variable emissions scaling factors.
#' @param nar2 the number of area sources with variable location and emission parameters.
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
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("NAR1", "IARU", "NSAR1", "NAR2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(nar1, iaru, nsar1, nar2)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
