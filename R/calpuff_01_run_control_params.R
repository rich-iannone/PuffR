#' Set CALPUFF run control parameters
#' @description This function validates and writes CALPUFF run control parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param metrun the choice of whether to run all periods available in meteorological input file.
#' @param ibyr the starting year for the CALPUFF run.
#' @param ibmo the starting month for the CALPUFF run.
#' @param ibdy the starting day for the CALPUFF run.
#' @param ibhr the starting hour for the CALPUFF run.
#' @param ibsec the starting second for the CALPUFF run.
#' @param ieyr the ending year for the CALPUFF run.
#' @param iemo the ending month for the CALPUFF run.
#' @param iedy the ending day for the CALPUFF run.
#' @param iehr the ending hour for the CALPUFF run.
#' @param iesec the ending second for the CALPUFF run.
#' @param abtz the time zone for the CALPUFF domain.
#' @param nsecdt the number of seconds between time steps.
#' @param nse number of stationary emission sources.
#' @param itest a flag to stop the model run after the setup phase, which is useful for checking for correctness in model inputs and associated files.
#' @param mrestart a flag to interact with restart files.
#' @param nrespd the number of periods in the restart output cycle.
#' @param metfm the meteorological data format to be used: (1) CALMET binary file (CALMET.DAT), (2) ISC ASCII file (ISCMET.DAT), (3) AUSPLUME ASCII file (PLMMET.MET), (4) CTDM plus tower file (PROFILE.DAT) and surface parameters file (SURFACE.DAT), or (5) AERMET tower file (PROFILE.DAT) and surface parameters file (SURFACE.DAT).
#' @param mprffm the meteorological profile data format (ignored unless metfm is either 1, 2, or 3): (1) CTDM plus tower file, or (2) AERMET tower file.
#' @param avet the averaging time in minutes.
#' @param pgtime the PG averaging time in minutes.
#' @param ioutu the output units for binary concentration and flux files: (1) mass units (e.g., g/m3 or g/m2/s), (2) odour units, or (3) radiation units (e.g., Bq/m3 or Bq/m2/s).
#' @param iovers the output dataset format version for binary concentration and flux files: (1) version 2.1, or (2) version 2.2.
#' @export calpuff_01_run_control_params

calpuff_01_run_control_params <- function(calpuff_inp = "calpuff_template.txt",
                                          read_data_from_surf_dat = TRUE,
                                          metrun = TRUE,
                                          ibyr = NULL,
                                          ibmo = NULL,
                                          ibdy = NULL,
                                          ibhr = NULL,
                                          ibmin = 0,
                                          ibsec = 0,
                                          ieyr = NULL,
                                          iemo = NULL,
                                          iedy = NULL,
                                          iehr = NULL,
                                          iemin = 0,
                                          iesec = 0,
                                          abtz = NULL,
                                          nsecdt = 3600,
                                          nspec = NULL,
                                          nse = NULL,
                                          itest = 2,
                                          mrestart = 0,
                                          nrespd = 0,
                                          metfm = 1,
                                          mprffm = 1,
                                          avet = 60.0,
                                          pgtime = 60.0,
                                          ioutu = 1,
                                          iovers = 2){
  
  # Add require statement
  require(lubridate)
  
  # Transform TRUE or FALSE value for 'metrun' to string
  metrun <- ifelse(metrun == TRUE, "T", "F")
  
  # Change NULL values for certain arguments to NA values
  if (is.null(ibyr)) ibyr <- NA
  if (is.null(ibmo)) ibmo <- NA
  if (is.null(ibdy)) ibdy <- NA
  if (is.null(ibhr)) ibhr <- NA
  if (is.null(ieyr)) ieyr <- NA
  if (is.null(iemo)) iemo <- NA
  if (is.null(iedy)) iedy <- NA
  if (is.null(iehr)) iehr <- NA
  if (is.null(abtz)) abtz <- NA
  if (is.null(nspec)) nspec <- NA
  if (is.null(nse)) nse <- NA
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("METRUN", "IBYR", "IBMO", "IBDY", "IBHR", "IBMIN", "IBSEC",
                "IEYR", "IEMO", "IEDY", "IEHR", "IEMIN", "IESEC",
                "ABTZ", "NSECDT", "NSPEC", "NSE", "ITEST", "MRESTART",
                "NRESPD", "METFM", "MPRFFM", "AVET", "PGTIME",
                "IOUTU", "IOVERS")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(metrun, ibyr, ibmo, ibdy, ibhr, ibmin, ibsec,
                    ieyr, iemo, iedy, iehr, iemin, iesec,
                    abtz, nsecdt, nspec, nse, itest, mrestart,
                    nrespd, metfm, mprffm, avet, pgtime,
                    ioutu, iovers)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
