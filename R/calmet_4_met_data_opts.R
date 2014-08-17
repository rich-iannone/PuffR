#' Set CALMET parameters for the meteorological data options
#' @description This function validates and writes CALMET parameters for the meteorological data options to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @export calmet_4_met_data_opts

calmet_4_met_data_opts <- function(calmet_inp,
                                   noobs = 0,
                                   nssta,
                                   npsta,
                                   icloud = 0,
                                   iforms = 2,
                                   iformp = 2,
                                   iformc = 2){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("NOOBS", "NSSTA", "NPSTA", "ICLOUD", "IFORMS", "IFORMP", "IFORMC")  
  
  
}
