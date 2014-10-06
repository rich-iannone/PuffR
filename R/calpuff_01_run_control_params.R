#' Set CALPUFF run control parameters
#' @description This function validates and writes CALPUFF run control parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param metrun 
#' @param ibyr the starting year for the CALPUFF run.
#' @param ibmo the starting month for the CALPUFF run.
#' @param ibdy the starting day for the CALPUFF run.
#' @param ibhr the starting hour for the CALPUFF run.
#' @param ibsec the starting second for the CALPUFF run.
#' @param ieyr the ending year for the CALPUFF run.
#' @param iemo the ending month for the CALPUFF run.
#' @param iedy the ending day for the CALPUFF run.
#' @param iehr the ending hour for the CALPUFF run.
#' @param iesec the ending second for the CALPUFF run.
#' @param abtz the time zone for the CALPUFF domain.
#' @param nsecdt the number of seconds between time steps.
#' @param nse 
#' @param itest
#' @param mrestart
#' @param nrespd
#' @param metfm
#' @param mprffm
#' @param avet
#' @param pgtime
#' @param ioutu
#' @param iovers
#' @export calpuff_01_run_control_params

calpuff_01_run_control_params <- function(calpuff_inp = "calpuff_template.txt",
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
                                          nse,
                                          itest,
                                          mrestart,
                                          nrespd,
                                          metfm,
                                          mprffm,
                                          avet,
                                          pgtime,
                                          ioutu,
                                          iovers){
  
  # Add require statement
  require(lubridate)
  
}
