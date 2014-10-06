#' Set CALMET temporal and model run parameters
#' @description This function validates and writes CALMET parameters for the model starting and ending times, the time zone, and model timings.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param read_data_from_surf_dat an option to read the time variable data from an extant SURF.DAT file in the working folder.
#' @param ibyr the starting year for the CALMET run.
#' @param ibmo the starting month for the CALMET run.
#' @param ibdy the starting day for the CALMET run.
#' @param ibhr the starting hour for the CALMET run.
#' @param ibsec the starting second for the CALMET run.
#' @param ieyr the ending year for the CALMET run.
#' @param iemo the ending month for the CALMET run.
#' @param iedy the ending day for the CALMET run.
#' @param iehr the ending hour for the CALMET run.
#' @param iesec the ending second for the CALMET run.
#' @param abtz the time zone for the CALMET domain.
#' @param nsecdt the number of seconds between time steps.
#' @param irtype the CALMET run type, where the '0' option computes wind fields only and the '1' option computes wind fields and several micrometeorological variables (U*, W*, L, Zi, etc.).
#' @param lcalgrd the option of whether to compute special data fields required by CALGRID.
#' @param itest a flag to stop the model run after the setup phase, which is useful for checking for correctness in model inputs and associated files.
#' @param mreg choice of whether to use EPA regulatory options.
#' @export calmet_01_temporal_params

calmet_01_temporal_params <- function(calmet_inp = "calmet_template.txt",
                                      read_data_from_surf_dat = TRUE,
                                      ibyr = NULL,
                                      ibmo = NULL,
                                      ibdy = NULL,
                                      ibhr = NULL,
                                      ibsec = 0,
                                      ieyr = NULL,
                                      iemo = NULL,
                                      iedy = NULL,
                                      iehr = NULL,
                                      iesec = 0,
                                      abtz = NULL,
                                      nsecdt = 3600,
                                      irtype = 1,
                                      lcalgrd = TRUE,
                                      itest = 2,
                                      mreg = 0){
  
  # Add require statement
  require(lubridate)
  
  # Transform TRUE or FALSE value for 'lcalgrd' to string
  lcalgrd <- ifelse(lcalgrd == TRUE, "T", "F")
  
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
  
  # If option set to read data from SURF.DAT file, get the relevant values
  if (read_data_from_surf_dat == TRUE){
    
    # Generate vector list of all SURF.DAT files in the working folder
    surf_dat_file <- list.files(pattern = "surf--.*")
    
    # If there are multiple SURF.DAT files in the working folder, choose only
    # the first of the set
    if (length(surf_dat_file > 1)) surf_dat_file <- surf_dat_file[1]
    
    # Obtain several lines from the header portion of the SURF.DAT file
    surf_dat_header <- readLines(surf_dat_file, warn = FALSE)[
      (as.numeric(readLines(surf_dat_file, warn = FALSE)[2]) + 4):
        (as.numeric(readLines(surf_dat_file, warn = FALSE)[2]) + 5)]
    
    # Get the UTC time zone
    abtz <- surf_dat_header[1]
    
    # Get the time data
    ibyr <- gsub("[ ]*([0-9]*).*", "\\1",
                 surf_dat_header[2])
    
    ibjulday <- gsub("[ ]*[0-9]*[ ]*([0-9]*).*", "\\1",
                     surf_dat_header[2])
    
    ibhr <- gsub("[ ]*[0-9]*[ ]*[0-9]*[ ]*([0-9]*).*", "\\1",
                 surf_dat_header[2])
    
    ieyr <- gsub("[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*[ ]*([0-9]*).*", "\\1",
                 surf_dat_header[2])
    
    iejulday <- gsub(paste0("[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*",
                            "[ ]*[0-9]*[ ]*([0-9]*).*"), "\\1",
                     surf_dat_header[2])
    
    iehr <- gsub(paste0("[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*[ ]*[0-9]*",
                        "[ ]*[0-9]*[ ]*[0-9]*[ ]*([0-9]*).*"), "\\1",
                 surf_dat_header[2])
    
    ibmo <- month(ISOdatetime(as.numeric(ibyr),
                              month = 1, day = 1, hour = 0, min = 0, sec = 0,
                              tz = "GMT") + ((as.numeric(ibjulday) - 1) * (3600 * 24)))
    
    ibdy <- day(ISOdatetime(as.numeric(ibyr),
                            month = 1, day = 1, hour = 0, min = 0, sec = 0,
                            tz = "GMT") + ((as.numeric(ibjulday) - 1) * (3600 * 24)))
    
    iemo <- month(ISOdatetime(as.numeric(ieyr),
                              month = 1, day = 1, hour = 0, min = 0, sec = 0,
                              tz = "GMT") + ((as.numeric(iejulday) - 1) * (3600 * 24)))
    
    iedy <- day(ISOdatetime(as.numeric(ieyr),
                            month = 1, day = 1, hour = 0, min = 0, sec = 0,
                            tz = "GMT") + ((as.numeric(iejulday) - 1) * (3600 * 24)))
  }
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
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
