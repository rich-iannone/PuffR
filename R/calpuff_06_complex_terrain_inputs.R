#' Set the CALPUFF subgrid scale complex terrain inputs
#' @description This function validates and writes CALPUFF subgrid scale complex terrain inputs.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param nhill 
#' @param nctrec 
#' @param mhill 
#' @param xhill2m 
#' @param zhill2m 
#' @param xctdmkm 
#' @param yctdmkm 
#' @export calpuff_06_complex_terrain_inputs

calpuff_06_complex_terrain_inputs <- function(calpuff_inp = "calpuff_template.txt",
                                              nhill,
                                              nctrec,
                                              mhill,
                                              xhill2m,
                                              zhill2m,
                                              xctdmkm,
                                              yctdmkm){
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("NHILL", "NCTREC", "MHILL",
                "XHILL2M", "ZHILL2M", "XCTDMKM", "YCTDMKM")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(nhill, nctrec, mhill,
                    xhill2m, zhill2m, xctdmkm, yctdmkm)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
