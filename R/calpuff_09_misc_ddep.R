#' Set miscellaneous CALPUFF parameters for dry deposition
#' @description This function validates and writes miscellaneous CALPUFF parameters for dry deposition.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param rcutr the reference cuticle resistance in units of s/cm.
#' @param rgr rcutr the reference ground resistance in units of s/cm.
#' @param reactr the reference pollutant reactivity.
#' @param nint the number of particle-size intervals used to evaluate effective particle deposition velocity.
#' @param iveg the choice of vegetation state in unirrigated areas: (1) active and unstressed vegetation, (2) active and stressed vegetation, or (3) inactive vegetation.
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
