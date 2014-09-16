#' Finalize and generate CALMET input files
#' @description This function sets the input and output filenames for CALMET model runs generates one or a series of CALMET.INP files.
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
#' @export calmet_inp_finalize

calmet_inp_finalize <- function(calmet_inp = "calmet_template.txt",
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
                                nowsta = 0,
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
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp)
  
  # Get file information from working folder if the option is taken
  if (get_filenames_from_wd == TRUE){
    
    # Generate a list of GEO.DAT files that are available in folder
    geodat_files <- list.files(pattern = "geo--.*")
    
    # Determine number of GEO.DAT files available in folder
    number_geodat_year <- length(geodat_files)
    
    # If there is at least one GEO.DAT file, provide a TRUE value for the geodat object
    if (number_geodat_year > 0) geodat <- TRUE
    if (number_geodat_year == 0) geodat <- FALSE
    
    # The number of GEO.DAT files will dictate the number of CALMET input files
    # that need to be generated per year
    number_srfdat_per_year <- number_geodat_year
    
    # Generate a list of SURF.DAT files that are available in folder
    srtdat_files <- list.files(pattern = "surf--.*")
    
    # Determine the number of SURF.DAT files, each representing a year's worth
    # of data
    number_srfdat_years <- length(srtdat_files)
    
    # If there is at least one SURF.DAT file, provide a TRUE value for the srfdat object
    if (number_srfdat_years > 0) srfdat <- TRUE
    if (number_srfdat_years == 0) srfdat <- FALSE
    
    # Determine the total number of CALMET.INP files that need to be made
    number_calmet_input_files_to_make <- number_srfdat_per_year * number_srfdat_years
    
    # Generate a list of CLOUD.DAT files that are available in folder
    clddat_files <- list.files(pattern = "cloud--.*")
    
    # Determine number of CLOUD.DAT files available in folder
    number_clddat_files <- length(clddat_files)
    
    # Generate a list of PRECIP.DAT files that are available in folder
    prcdat_files <- list.files(pattern = "precip--.*")
    
    # Determine number of PRECIP.DAT files available in folder
    number_prcdat_files <- length(prcdat_files)
    
    # Generate a list of WT.DAT files that are available in folder
    wtdat_files <- list.files(pattern = "wt--.*")
    
    # Determine number of WT.DAT files available in folder
    number_wtdat_files <- length(wtdat_files)
    
    
    
  }
  

  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}

