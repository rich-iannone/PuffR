#' Set CALMET parameters for the meteorological data options
#' @description This function validates and writes CALMET parameters for the meteorological data options to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param noobs option for 'no observation mode' where 0 represents the use of surface, overwater, and upper air stations; 1 represents the use of surface and overwater stations (no upper air observations), and MM4/MM5/3D data files for upper air information; and 3 represents having no surface, overwater, or upper air observations used (use instead MM4/MM5/3D data files for surface, overwater, and upper air data).
#' @param nssta number of surface meteorological stations used.
#' @param npsta number of precipitation stations used; a value of -1 is to be provided when MM5/3D precip data is used.
#' @param icloud represents the cloud data option to use, where 0 indicates that gridded cloud information is not to be used; 1 represents the option for gridded cloud.dat file to be generated as output; the use of option 2 states that the gridded cloud.dat file will be read in as input; and option 3 is for computing gridded cloud cover from prognostic fields.
#' @param iforms represents two possible options for the surface meteorological data file format: 1 - unformatted (e.g., SMERGE output); 2 - formatted (as free-formatted user input).
#' @param iformp represents two possible options for the precipitation data file format: 1 - unformatted (e.g., PMERGE output); 2 - formatted (free-formatted user input).
#' @param iformc represents two possible options for the cloud data file format: 1 - unformatted as CALMET unformatted output; 2 - formatted (free-formatted CALMET output or user input).
#' @export calmet_04_met_data_opts

calmet_04_met_data_opts <- function(calmet_inp,
                                    noobs = 0,
                                    nssta,
                                    npsta,
                                    icloud = 0,
                                    iforms = 2,
                                    iformp = 2,
                                    iformc = 2){
  
  # Change NULL values for certain arguments to NA values
  if (is.null(nssta)) nssta <- NA
  if (is.null(npsta)) npsta <- NA
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("NOOBS", "NSSTA", "NPSTA", "ICLOUD", "IFORMS", "IFORMP", "IFORMC")  
  
  # Generate a vector list of the formatted replacements
  replacements <- c(noobs, nssta, npsta, icloud, iforms, iformp, iformc)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
