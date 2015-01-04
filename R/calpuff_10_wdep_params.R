#' Set the CALPUFF parameters for wet deposition
#' @description This function validates and writes CALPUFF parameters for wet deposition.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param species_scaveng_coeff_liquid_precip 
#' @param species_scaveng_coeff_froze_precip 
#' @export calpuff_10_wdep_params

calpuff_10_wdep_params <- function(calpuff_inp = "calpuff_template.txt",
                                   species_scaveng_coeff_liquid_precip = NULL,
                                   species_scaveng_coeff_froze_precip = NULL){
  
  # Generate default list of values for the scavenging coefficients when wet
  # deposition occurs via either liquid or frozen precipitation for a large number
  # of chemical species
  default_params_wdep <- list(so2 = c(0.000030, 0)
                              so4 = c(0.000100, 0.000030),
                              no = c(0.000029, 0),
                              no2 = c(0.000510, 0),
                              hno3 = c(0.000060, 0),
                              no3 = c(0.000100, 0.000030),
                              pm25 = c(0.000100, 0.000030),
                              pm10 = c(0.000100, 0.000030))
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
}
