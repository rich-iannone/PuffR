#' Set the CALPUFF chemistry parameters
#' @description This function validates and writes CALPUFF parameters for atmospheric chemistry.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param moz the choice of ozone data input (ignored unless mchem is 1, 3, 4, 6, or 7): (0) use a monthly background ozone option, or (1) read hourly ozone concentrations from an OZONE.DAT file.
#' @param bcko3 monthly ozone concentrations as ppbV values (ignored unless mchem is 1, 3, 4, 6, or 7, and, moz is 0 or 1).
#' @param mnh3 the choice of ammonia data input (ignored unless mchem is 6 or 7): (0) use monthly background ammonia values, or (1) read hourly ammonia concentrations for each vertical layer from a NH3Z.DAT file.
#' @param mavgnh3 the choice of ammonia vertical averaging method (ignored unless mchem is 6 or 7, and, mnh3 is 1): (0) do not calculate averages and use NH3 at puff center height, or (1) average NH3 values over vertical extent of puff.
#' @param bcknh3 monthly ammonia concentrations as ppbV values (ignored unless mchem is 1 or 3, or, mchem is 6 or 7 and mnh3 is 0).
#' @param rnite1 the nighttime SO2 loss rate in percent per hour. This rate is used only at night for when mchem is set to 1. This rate is added to the computed rate both day and night when mchem is either 6 or 7 for heterogeneous reaction chemistry.
#' @param rnite2 the nighttime NOx loss rate in percent per hour (ignored unless mchem is set to 1).
#' @param rnite3 the nighttime HNO3 formation rate in percent per hour (ignored unless mchem is set to 1).
#' @param mh2o2 the choice of ammonia data input (ignored unless mchem is 6 or 7): (0) use a monthly background H2O2 option, or (1) read hourly H2O2 concentrations from an H2O2.DAT file.
#' @param bckh2o2 monthly H2O2 concentrations as ppbV values (ignored unless mchem is 1 and either mh2o2 is 0 or mh2o2 is 1 and all hourly H2O2 data is missing).
#' @param bckpmf 
#' @param ofrac 
#' @param vcnx 
#' @param ndecay 
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
                                            50.0, 50.0, 50.0, 50.0, 50.0, 50.0)){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Provide a value of 0 for 'ndecay'
  ndecay <- 0
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("MOZ", "BCKO3", "MNH3", "MAVGNH3", "BCKNH3", "RNITE1", "RNITE2", "RNITE3",
                "MH2O2", "BCKH2O2", "BCKPMF", "OFRAC", "VCNX", "NDECAY")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(moz, bcko3, mnh3, mavgnh3, bcknh3, rnite1, rnite2, rnite3,
                    mh2o2, bckh2o2, bckpmf, ofrac, vcnx, ndecay)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
