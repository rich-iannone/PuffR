#' Set input and output filenames for CALMET model run
#' @description This function sets the input and output filenames for CALMET model runs in the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param get_filenames_from_wd 
#' @param geodat  
#' @param srfdat  
#' @param clddat  
#' @param prcdat  
#' @param wtdat  
#' @param metlst  
#' @param metdat  
#' @param pacdat  
#' @param lcfiles 
#' @param nusta  
#' @param nowsta  
#' @param nm3d  
#' @param nigf  
#' @param updat  
#' @param seadat  
#' @param m3ddat  
#' @param igfdat  
#' @param diadat  
#' @param prgdat  
#' @param tstprt  
#' @param tstout  
#' @param tstkin  
#' @param tstfrd  
#' @param tstslp  
#' @param dcstgd 
#' @export calmet_00_filenames

calmet_00_filenames <- function(calmet_inp = "calmet_template.txt",
                                get_filenames_from_wd = TRUE,
                                geodat = NULL,
                                srfdat = NULL,
                                clddat = NULL,
                                prcdat = NULL,
                                wtdat = NULL,
                                metlst = NULL,
                                metdat = NULL,
                                pacdat = NULL,
                                lcfiles = TRUE,
                                nusta = NULL,
                                nowsta = NULL,
                                nm3d = NULL,
                                nigf = NULL,
                                updat = NULL,
                                seadat = NULL,
                                m3ddat = NULL,
                                igfdat = NULL,
                                diadat = NULL,
                                prgdat = NULL,
                                tstprt = NULL,
                                tstout = NULL,
                                tstkin = NULL,
                                tstfrd = NULL,
                                tstslp = NULL,
                                dcstgd = NULL){
    
  # Get file information from working folder if the option is taken
  if (get_filenames_from_wd == TRUE){
    
    
  }
  
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp)
    
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}

