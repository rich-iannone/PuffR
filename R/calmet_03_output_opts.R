#' Set CALMET parameters for the output options
#' @description This function validates and writes CALMET parameters for the output options to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param lsave save meteorological fields in an unformatted output file.
#' @param iformo type of unformatted output file: (1) CALPUFF/CALGRID type file [CALMET.DAT], (2) MESOPUFF-II type file [PACOUT.DAT].
#' @param lprint whether to print meteorological fields in the iuvout, iwout, and itout options.
#' @param iprinf print interval in hours.
#' @param iuvout specification of which layers of U,V wind component to print; vector must be the same length as the number of levels (nz).
#' @param iwout specification of which layers of the W wind component to print; vector must be the same length as the number of levels (nz).
#' @param itout specification of which levels of the 3D temperature field to print; vector must be the same length as the number of levels (nz).
#' @param stability whether to print the PGT stability class (will only print if lprint is TRUE).
#' @param ustar whether to print the friction velocity (will only print if lprint is TRUE).
#' @param monin whether to print the Monin-Obukhov length (will only print if lprint is TRUE).
#' @param mixht whether to print the mixing height (will only print if lprint is TRUE).
#' @param wstar whether to print the convective velocity scale (will only print if lprint is TRUE).
#' @param precip whether to print the precipitation rate (will only print if lprint is TRUE).
#' @param sensheat whether to print the sensible heat flux (will only print if lprint is TRUE).
#' @param convzi whether to print the convective mixing height (will only print if lprint is TRUE).
#' @param ldb whether to print input meteorological data and internal variables.
#' @param nn1 if ldb is TRUE, the first time step for which debug data are printed.
#' @param nn2 if ldb is TRUE, the last time step for which debug data are printed.
#' @param ldbcst whether to print distance to land internal variables (output will be a .GRD file DCST.GRD).
#' @param ioutd whether to print the test/debug wind fields to disk files.
#' @param nzprn2 number of levels, starting at the surface, to print.
#' @param ipr0 print the interpolated wind components?
#' @param ipr1 print the terrain-adjusted surface wind components?
#' @param ipr2 print the smoothed wind components and the initial divergence fields?
#' @param ipr3 print the final wind speed and direction fields?
#' @param ipr4 print the final divergence fields?
#' @param ipr5 print the winds after kinematic effects are added?
#' @param ipr6 print the winds after the Froude number adjustment is made?
#' @param ipr7 print the winds after slope flows are added?
#' @param ipr8 print the final wind field components?
#' @export calmet_03_output_opts

calmet_03_output_opts <- function(calmet_inp = "calmet_template.txt",
                                  lsave = TRUE,
                                  iformo = 0,
                                  lprint = FALSE,
                                  iprinf = 1,
                                  iuvout = NULL,
                                  iwout = NULL,
                                  itout = NULL,
                                  stability = TRUE,
                                  ustar = TRUE,
                                  monin = TRUE,
                                  mixht = TRUE,
                                  wstar = TRUE,
                                  precip = TRUE,
                                  sensheat = TRUE,
                                  convzi = TRUE,
                                  ldb = TRUE,
                                  nn1 = 1,
                                  nn2 = 24,
                                  ldbcst = FALSE,
                                  ioutd = FALSE,
                                  nzprn2 = FALSE,
                                  ipr0 = FALSE,
                                  ipr1 = FALSE,
                                  ipr2 = FALSE,
                                  ipr3 = FALSE,
                                  ipr4 = FALSE,
                                  ipr5 = FALSE,
                                  ipr6 = FALSE,
                                  ipr7 = FALSE,
                                  ipr8 = FALSE){
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
  # Transform TRUE or FALSE value for 'lsave' to string
  lsave <- ifelse(lsave == TRUE, "T", "F")

  # Transform TRUE or FALSE value for 'lprint' to string
  lprint <- ifelse(lprint == TRUE, "T", "F")

  # Transform TRUE or FALSE value for 'ldb' to string
  ldb <- ifelse(ldb == TRUE, "T", "F")

  # Transform TRUE or FALSE value for 'ldbcst' to string
  ldbcst <- ifelse(ldbcst == TRUE, "T", "F")
    
  # Generate a vector list of calmet.inp keywords that require boolean values
  keywords <- c("LSAVE", "LPRINT", "LDB", "LDBCST")
  
  # Generate a vector list of the formatted boolean value replacements
  replacements <- c(lsave, lprint, ldb, ldbcst)
  
  # Modify all parameters that require boolean values in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
  # Get number of layers
  nz <- as.numeric(gsub(".*=(.*)!", "\\1",
                        grep(paste("NZ(?![[:alpha:]])", sep = ''),
                             calmet_inp_working, perl = TRUE, value = TRUE)))
  
  # If 'nz' value can be determined (and 'iuvout', 'iwout', and 'itout' arguments are
  # NULL), then generate default vectors of 'nz' length and transform to strings
  if(!is.na(nz)){
    if (is.null(iuvout)) iuvout <- paste(c(1, rep(0, (nz - 1))), collapse = ", ")
    if (is.null(iwout)) iwout <- paste(c(1, rep(0, (nz - 1))), collapse = ", ")
    if (is.null(itout)) itout <- paste(c(1, rep(0, (nz - 1))), collapse = ", ")
  }
  
  # Generate NA values for 'iuvout', 'iwout', and 'itout' if 'nz' is NA
  if(is.na(nz)){
    if (is.null(iuvout)) iuvout <- NA
    if (is.null(iwout)) iwout <- NA
    if (is.null(itout)) itout <- NA
  }
  
  # Add formatted 'iuvout' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "IUVOUT",
                                       replacement = iuvout)
  
  # Add formatted 'iwout' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "IWOUT",
                                       replacement = iwout)
  
  # Add formatted 'itout' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "ITOUT",
                                       replacement = itout)
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
  # Generate a vector list of calmet.inp keywords that require numeric values
  keywords <- c("IFORMO", "IPRINF", "STABILITY", "USTAR", "MONIN", "MIXHT",
                "WSTAR", "PRECIP", "SENSHEAT", "CONVZI", "NN1", "NN2", "IOUTD", "NZPRN2",
                "IPR0", "IPR1", "IPR2", "IPR3", "IPR4", "IPR5", "IPR6", "IPR7", "IPR8")
  
  # Generate a vector list of the formatted single numeric value replacements
  replacements <- c(iformo, iprinf, stability, ustar, monin, mixht,
                    wstar, precip, sensheat, convzi, nn1, nn2, ioutd, nzprn2,
                    ipr0, ipr1, ipr2, ipr3, ipr4, ipr5, ipr6, ipr7, ipr8)
  
  # Modify all parameters that require single numeric values in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements) 
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
