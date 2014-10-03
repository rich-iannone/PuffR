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
