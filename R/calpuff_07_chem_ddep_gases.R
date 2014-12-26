#' Set the CALPUFF chemical parameters for dry deposition of gases
#' @description This function validates and writes CALPUFF chemical parameters for dry deposition of gases.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @export calpuff_07_chem_ddep_gases

calpuff_07_chem_ddep_gases <- function(calpuff_inp = "calpuff_template.txt",
                                       species_names = NULL,
                                       species_ddep_params = NULL){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
}
