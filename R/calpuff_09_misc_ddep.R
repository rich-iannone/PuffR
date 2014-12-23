#' Set miscellaneous CALPUFF parameters for dry deposition
#' @description This function validates and writes miscellaneous CALPUFF parameters for dry deposition.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param rcutr 
#' @param rgr 
#' @param reactr 
#' @param nint 
#' @param iveg 
#' @export calpuff_09_misc_ddep

calpuff_09_misc_ddep <- function(calpuff_inp = "calpuff_template.txt",
                                 rcutr = 30.0,
                                 rgr = 5.0,
                                 reactr = 8.0,
                                 nint = 9,
                                 iveg = 1){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("RCUTR", "RGR", "REACTR", "NINT", "IVEG")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(rcutr, rgr, reactr, nint, iveg)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
