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
#' @export calmet_3_output_opts

calmet_3_output_opts <- function(calmet_inp,
                                 lsave = TRUE,
                                 iformo = 0,
                                 lprint = FALSE,
                                 iprinf = 1,
                                 iuvout = c(1, rep(0, (nz - 1))),
                                 iwout = c(1, rep(0, (nz - 1))),
                                 itout = c(1, rep(0, (nz - 1))),
                                 stability = FALSE,
                                 ustar = FALSE,
                                 monin = FALSE,
                                 mixht = FALSE,
                                 wstar = FALSE,
                                 precip = FALSE,
                                 sensheat = FALSE,
                                 convzi = FALSE,
                                 ldb = FALSE,
                                 nn1 = 1,
                                 nn2 = 1,
                                 ldbcst = FALSE,
                                 ioutd = FALSE,
                                 nzprn2 = TRUE,
                                 ipr0 = FALSE,
                                 ipr1 = FALSE,
                                 ipr2 = FALSE,
                                 ipr3 = FALSE,
                                 ipr4 = FALSE,
                                 ipr5 = FALSE,
                                 ipr6 = FALSE,
                                 ipr7 = FALSE,
                                 ipr8 = FALSE){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("LSAVE", "IFORMO", "LPRINT", "IPRINF", "IUVOUT", "IWOUT", "ITOUT", "STABILITY",
                "USTAR", "MONIN", "MIXHT", "WSTAR", "PRECIP", "SENSHEAT", "CONVZI",
                "LDB", "NN1", "NN2", "LDBCST", "IOUTD", "NZPRNZ",
                "IPR0", "IPR1", "IPR2", "IPR3", "IPR4", "IPR5", "IPR6", "IPR7", "IPR8")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(lsave, iformo, lprint, iprinf, iuvout, iwout, itout,
                    stability, ustar, monin, mixht, wstar, precip, sensheat, convzi,
                    ldb, nn1, nn2, ldbcst, ioutd, nzprnz,
                    ipr0, ipr1, ipr2, ipr3, ipr4, ipr5, ipr6, ipr7, ipr8)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
  
}
