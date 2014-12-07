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
                                   moz,
                                   bcko3,
                                   mnh3,
                                   mavgnh3,
                                   bcknh3,
                                   rnite1,
                                   rnite2,
                                   rnite3,
                                   mh2o2,
                                   bckh2o2,
                                   bckpmf,
                                   ofrac,
                                   vcnx,
                                   ndecay,
                                   spec1,
                                   spec2){
  
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
