#' Set the CALPUFF size parameters for dry deposition of particles
#' @description This function validates and writes CALPUFF size parameters for dry deposition of particles.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param species_names a vector list of species for which dry deposition size parameters will be supplied.
#' @param species_geom_mass_mean_diam a vector list of geometric mass mean diameters (in micron units) for each of the species provided in the 'species_names' object.
#' @param species_geom_SD a vector list of geometric standard deviation values (in micron units) for each of the species provided in the 'species_names' object.
#' @export calpuff_08_size_ddep_particles

calpuff_08_size_ddep_particles <- function(calpuff_inp = "calpuff_template.txt",
                                           species_names = NULL,
                                           species_geom_mass_mean_diam = NULL,
                                           species_geom_SD = NULL){
  
  # Generate default list of values for the geometric mass mean diameter and the
  # geometric standard deviation (both in units of microns) for a large number of
  # chemical species
  default_params_ddep <- list(so4 = c(0.48, 2.0),
                              no3 = c(0.48, 2.0),
                              pm25 = c(0.48, 2.0),
                              pm10 = c(0.48, 2.0))
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  
}
