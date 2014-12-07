#' Set one or more CALPUFF line source parameters
#' @description This function validates and writes CALPUFF line source parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param nln2 
#' @param nlines 
#' @param ilnu 
#' @param nsln1 
#' @param mxnseg 
#' @param nlrise 
#' @param xl 
#' @param hbl 
#' @param wbl 
#' @param wml 
#' @param dxl 
#' @param fprimel
#' @param lnsource_params 
#' @export calpuff_15_line_source_params

calpuff_15_line_source_params <- function(calpuff_inp = "calpuff_template.txt",
                                          nln2 = NULL,
                                          nlines = NULL,
                                          ilnu = NULL,
                                          nsln1 = NULL,
                                          mxnseg = NULL,
                                          nlrise = NULL,
                                          xl = NULL,
                                          hbl = NULL,
                                          wbl = NULL,
                                          wml = NULL,
                                          dxl = NULL,
                                          fprimel = NULL,
                                          lnsource_params = NULL){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("NLN2", "NLINES", "ILNU", "NSLN1", "MXNSEG", "NLRISE",
                "XL", "HBL", "WBL", "WML", "DXL", "FPRIMEL")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(nln2, nlines, ilnu, nsln1, mxnseg, nlrise,
                    xl, hbl, wbl, wml, dxl, fprimel)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
