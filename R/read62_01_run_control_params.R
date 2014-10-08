#' Set the READ62 run control parameters
#' @description This function validates and writes READ62 run control parameters to the working READ62.INP file.
#' @param read62_inp the absolute path and filename for the working CALMET input file.
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
#' @param jdat the type of NCDC input sounding data file; where '1' is the TD-6201 format and '2' is the NCDC FSL format.
#' @param isub the format of substitute UP.DAT input sounding data file; where '0' indicates that no substitute will be used, '1' states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations), and '2' states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations). 
#' @param ifmt the format of the main UP.DAT input sounding data file; where '1' states that the delimiter between sounding levels is a forward slash (and WS and WD have integer representations), and '2' states that the delimiter between sounding levels is a comma (and WS and WD have floating point representations).
#' @param pstop the top pressure level (in mb units) for which data are extracted. The pressure level must correspond to a height that equals or exceeds the top of the CALMET modeling domain, or else CALMET will stop with an error message.
#' @param lht a missing data control option for height that is used determine when a sounding level is rejected. If the height is missing from a level, that level will be rejected.
#' @param ltemp a missing data control option for temperature that is used determine when a sounding level is rejected. If the temperature is missing from a level, that level will be rejected.
#' @param lwd a missing data control option for wind direction that is used determine when a sounding level is rejected. If the wind direction is missing from a level, that level will be rejected.
#' @param lws a missing data control option for wind speed that is used determine when a sounding level is rejected. If the wind speed is missing from a level, that level will be rejected.
#' @param lxtop choice of whether to extrapolate to extend missing profile data to PSTOP pressure level.
#' @param pvtop if 'lxtop' is TRUE, then pvtop is the pressure level corresponding to where valid data must exist.
#' @param lxsfc choice of whether to extrapolate to extend missing profile data to the surface.
#' @param zvsfc if 'lxsfc' is TRUE, then zvsfc is the height (in meters) corresponding to where valid data must exist.
#' @export read62_01_run_control_params

read62_01_run_control_params <- function(read62_inp = "read62_template.txt",
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
                                         jdat = 2,
                                         isub = 2,
                                         ifmt = 2,
                                         pstop = 500,
                                         lht = FALSE,
                                         ltemp = FALSE,
                                         lwd = FALSE,
                                         lws = FALSE,
                                         lxtop = TRUE,
                                         pvtop = 850,
                                         lxsfc = TRUE,
                                         zvsfc = 200){
  
  # Add require statement
  require(lubridate)
  
  # Transform TRUE or FALSE value for 'lht' to string
  lht <- ifelse(lht == TRUE, "T", "F")
  
  # Transform TRUE or FALSE value for 'ltemp' to string
  ltemp <- ifelse(ltemp == TRUE, "T", "F")
  
  # Transform TRUE or FALSE value for 'lwd' to string
  lwd <- ifelse(lwd == TRUE, "T", "F")
  
  # Transform TRUE or FALSE value for 'lws' to string
  lws <- ifelse(lws == TRUE, "T", "F")
  
  # Transform TRUE or FALSE value for 'lxtop' to string
  lxtop <- ifelse(lxtop == TRUE, "T", "F")
  
  # Transform TRUE or FALSE value for 'lxsfc' to string
  lxsfc <- ifelse(lxsfc == TRUE, "T", "F")
  
  # Change NULL values for certain arguments to NA values
  if (is.null(ibyr)) ibyr <- NA
  if (is.null(ibmo)) ibmo <- NA
  if (is.null(ibdy)) ibdy <- NA
  if (is.null(ibhr)) ibhr <- NA
  if (is.null(ieyr)) ieyr <- NA
  if (is.null(iemo)) iemo <- NA
  if (is.null(iedy)) iedy <- NA
  if (is.null(iehr)) iehr <- NA
  
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
  read62_inp_working <- readLines(read62_inp, warn = FALSE)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("IBYR", "IBMO", "IBDY", "IBHR", "IBSEC",
                "IEYR", "IEMO", "IEDY", "IEHR", "IESEC",
                "JDAT", "ISUB", "IFMT", "PSTOP", 
                "LHT", "LTEMP", "LWD", "LWS",
                "LXTOP", "PVTOP", "LXSFC", "ZVSFC")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(ibyr, ibmo, ibdy, ibhr, ibsec,
                    ieyr, iemo, iedy, iehr, iesec,
                    jdat, isub, ifmt, pstop, 
                    lht, ltemp, lwd, lws,
                    lxtop, pvtop, lxsfc, zvsfc)
                    
  # Modify all parameters in working calmet.inp vector
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(read62_inp_working, con = read62_inp)
  
}
