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

calmet_01_temporal_params <- function(calmet_inp,
                                      ibyr,
                                      ibmo,
                                      ibdy,
                                      ibhr,
                                      ibsec,
                                      ieyr,
                                      iemo,
                                      iedy,
                                      iehr,
                                      iesec,
                                      abtz,
                                      nsecdt,
                                      irtype,
                                      lcalgrd,
                                      itest,
                                      mreg){
  
  
  
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
