#' Set the CALPUFF subgrid scale complex terrain inputs
#' @description This function validates and writes CALPUFF subgrid scale complex terrain inputs.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param nhill the number of terrain features.
#' @param nctrec the number of special complex terrain receptors.
#' @param mhill provenance of terrain and CTSG Receptor data: (1) hill and receptor data created by CTDM processors and read from HILL.DAT and HILLRCT.DAT files, or (2) hill data created by OPTHILL.
#' @param xhill2m factor to convert horizontal dimensions to meters.
#' @param zhill2m factor to convert vertical dimensions to meters.
#' @param xctdmkm the x-origin of the CTDM system relative to the CALPUFF coordinate system, in kilometers.
#' @param yctdmkm the y-origin of the CTDM system relative to the CALPUFF coordinate system, in kilometers.
#' @export calpuff_06_complex_terrain_inputs

calpuff_06_complex_terrain_inputs <- function(calpuff_inp = "calpuff_template.txt",
                                              nhill = 0,
                                              nctrec = 0,
                                              mhill = 2,
                                              xhill2m = 1.0,
                                              zhill2m = 1.0,
                                              xctdmkm = 0,
                                              yctdmkm = 0){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
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
