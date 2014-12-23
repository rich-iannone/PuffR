#' Set the CALPUFF chemistry parameters
#' @description This function validates and writes CALPUFF parameters for atmospheric chemistry.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param moz 
#' @param bcko3 
#' @param mnh3 
#' @param mavgnh3 
#' @param bcknh3 
#' @param rnite1 
#' @param rnite2 
#' @param rnite3 
#' @param mh2o2 
#' @param bckh2o2 
#' @param bckpmf 
#' @param ofrac 
#' @param vcnx 
#' @param ndecay 
#' @param spec1 
#' @param spec2 
#' @export calpuff_11_chem_params

calpuff_11_chem_params <- function(calpuff_inp = "calpuff_template.txt",
                                   moz = 1,
                                   bcko3 = c(40.0, 40.0, 40.0, 40.0, 40.0, 40.0,
                                             40.0, 40.0, 40.0, 40.0, 40.0, 40.0),
                                   mnh3 = 0,
                                   mavgnh3 = 1,
                                   bcknh3 = c(10.0, 10.0, 10.0, 10.0, 10.0, 10.0,
                                              10.0, 10.0, 10.0, 10.0, 10.0, 10.0),
                                   rnite1 = 0.2,
                                   rnite2 = 2.0,
                                   rnite3 = 2.0,
                                   mh2o2 = 1,
                                   bckh2o2 = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                               1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                                   bckpmf = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                              1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                                   ofrac = c(0.15, 0.15, 0.20, 0.20, 0.20, 0.20,
                                             0.20, 0.20, 0.20, 0.20, 0.20, 0.15),
                                   vcnx = c(50.0, 50.0, 50.0, 50.0, 50.0, 50.0,
                                            50.0, 50.0, 50.0, 50.0, 50.0, 50.0),
                                   ndecay = 0){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("MOZ", "BCKO3", "MNH3", "MAVGNH3", "BCKNH3", "RNITE1", "RNITE2", "RNITE3",
                "MH2O2", "BCKH2O2", "BCKPMF", "OFRAC", "VCNX", "NDECAY", "SPEC1", "SPEC2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(moz, bcko3, mnh3, mavgnh3, bcknh3, rnite1, rnite2, rnite3,
                    mh2o2, bckh2o2, bckpmf, ofrac, vcnx, ndecay, spec1, spec2)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
