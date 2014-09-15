#' Set CALMET temporal and model run parameters
#' @description This function validates and writes CALMET parameters for the model starting and ending times, the time zone, and model timings.
#' @param calmet_inp 
#' @param ibyr 
#' @param ibmo 
#' @param ibdy 
#' @param ibhr 
#' @param ibsec 
#' @param ieyr 
#' @param iemo 
#' @param iedy 
#' @param iehr 
#' @param iesec 
#' @param abtz 
#' @param nsecdt 
#' @param irtype 
#' @param lcalgrd 
#' @param itest 
#' @param mreg 
#' @export calmet_01_temporal_params

calmet_01_temporal_params <- function(calmet_inp = "calmet_template.txt",
                                      read_data_from_surf_dat = TRUE,
                                      ibyr = NULL,
                                      ibmo = NULL,
                                      ibdy = NULL,
                                      ibhr = NULL,
                                      ibsec = NULL,
                                      ieyr = NULL,
                                      iemo = NULL,
                                      iedy = NULL,
                                      iehr = NULL,
                                      iesec = NULL,
                                      abtz = NULL,
                                      nsecdt = 3600,
                                      irtype = 1,
                                      lcalgrd = TRUE,
                                      itest = 2,
                                      mreg = 0){
  
  # Add require statement
  require(lubridate)
  
  # Change NULL values for certain arguments to NA values
  if (is.null(ibyr)) ibyr <- NA
  if (is.null(ibmo)) ibmo <- NA
  if (is.null(ibdy)) ibdy <- NA
  if (is.null(ibhr)) ibhr <- NA
  if (is.null(ibsec)) ibsec <- NA
  if (is.null(ieyr)) ieyr <- NA
  if (is.null(iemo)) iemo <- NA
  if (is.null(iedy)) iedy <- NA
  if (is.null(iehr)) iehr <- NA
  if (is.null(iesec)) iesec <- NA
  if (is.null(abtz)) abtz <- NA
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("IBYR", "IBMO", "IBDY", "IBHR", "IBSEC",
                "IEYR", "IEMO", "IEDY", "IEHR", "IESEC",
                "ABTZ", "NSECDT", "IRTYPE", "LCALGRD", "ITEST", 
                "MREG")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(ibyr, ibmo, ibdy, ibhr, ibsec,
                    ieyr, iemo, iedy, iehr, iesec,
                    abtz, nsecdt, irtype, lcalgrd, itest,
                    mreg)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
