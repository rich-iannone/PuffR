#' Set READ62 run control parameters
#' @description This function validates and writes READ62 run control parameters to the working READ62.INP file.
#' @param read62_inp the absolute path and filename for the working CALMET input file.
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
                                         lxtop = TRUE,
                                         pvtop = 850,
                                         lxsfc = TRUE,
                                         zvsfc = 200){
  
  # Add require statement
  require(lubridate)
  
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
    
  }
  
  # Read in the working calmet.inp file as a character vector
  read62_inp_working <- readLines(read62_inp, warn = FALSE)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("IBYR", "IBMO", "IBDY", "IBHR", "IBSEC",
                "IEYR", "IEMO", "IEDY", "IEHR", "IESEC",
                "JDAT", "ISUB", "IFMT", "PSTOP", "LXTOP",
                "PVTOP", "LXSFC", "ZVSFC")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(ibyr, ibmo, ibdy, ibhr, ibsec,
                    ieyr, iemo, iedy, iehr, iesec,
                    jdat, isub, ifmt, pstop, lxtop,
                    pvtop, lxsfc, zvsfc)
                    
  # Modify all parameters in working calmet.inp vector
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(read62_inp_working, con = read62_inp)
  
}
