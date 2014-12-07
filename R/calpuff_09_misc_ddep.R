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
                                 rcutr,
                                 rgr,
                                 reactr,
                                 nint,
                                 iveg){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("RCUTR", "RGR", "REACTR", "NINT", "IVEG")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(rcutr, rgr, reactr, nint, iveg)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
