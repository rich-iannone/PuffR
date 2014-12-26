#' Set the CALPUFF chemical parameters for dry deposition of gases
#' @description This function validates and writes CALPUFF chemical parameters for dry deposition of gases.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param species_diffusivity 
#' @param species_alpha_star 
#' @param species_reactivity 
#' @param species_mesophyll_resistance 
#' @param species_HL_coefficient 
#' @export calpuff_07_chem_ddep_gases

calpuff_07_chem_ddep_gases <- function(calpuff_inp = "calpuff_template.txt",
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
}
