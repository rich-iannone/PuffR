#' Set one or more CALPUFF line source parameters
#' @description This function validates and writes CALPUFF line source parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param nln2 
#' @param nlines the number of buoyant line sources.
#' @param ilnu the units used for line source emissions, where the possible selections are: (1) "g/s", (2) "kg/hr", (3) "lb/hr", (4) "tons/yr", (5) "Odour Unit * m^3/s", (6) "Odour Unit * m^3/min", and (7) "metric tons/yr".
#' @param nsln1 
#' @param mxnseg 
#' @param nlrise the number of distances at which transitional rise is computed.
#' @param xl the average building length in meters.
#' @param hbl the average building height in meters.
#' @param wbl the average building width in meters.
#' @param wml the average line source width in meters.
#' @param dxl the average separation between buildings in meters.
#' @param fprimel the average buoyancy parameter, in units of m^4/s^3
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
