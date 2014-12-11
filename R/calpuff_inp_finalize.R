#' Finalize and generate CALPUFF input files
#' @description This function sets the input and output filenames for CALPUFF model runs generates one or a series of CALPUFF.INP files.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param get_filenames_from_wd 
#' @export calpuff_inp_finalize

calpuff_inp_finalize <- function(calpuff_inp = "calpuff_template.txt",
                                 get_filenames_from_wd = TRUE,
                                 ptdat = NULL,
                                 voldat = NULL,
                                 ardat = NULL,
                                 lndat = NULL,
                                 ozdat = NULL,
                                 vddat = NULL,
                                 chemdat = NULL,
                                 auxext = NULL,
                                 h2o2dat = NULL,
                                 nh3zdat = NULL,
                                 hildat = NULL,
                                 rctdat = NULL,
                                 cstdat = NULL,
                                 bdydat = NULL,
                                 bcndat = NULL,
                                 puflst = NULL,
                                 condat = NULL,
                                 dfdat = NULL,
                                 wfdat = NULL,
                                 visdat = NULL,
                                 t2ddat = NULL,
                                 rhodat = NULL,
                                 rstarte = NULL,
                                 debug = NULL,
                                 flxdat = NULL,
                                 baldat = NULL,
                                 fogdat = NULL,
                                 risdat = NULL,
                                 lcfiles = TRUE){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Transform TRUE or FALSE value for lcfiles to string
  lcfiles <- ifelse(lcfiles == TRUE, "T", "F")
  
  # Get file information from working folder if the option is taken
  if (get_filenames_from_wd == TRUE){
    
    # Generate a list of CALMET.INP files that are available in folder
    concdat_files <- list.files(pattern = "conc--.*")
    
    # Create a vector object of the CONC.DAT years
    for (i in 1:length(concdat_files)){
      if (i == 1) concdat_years <- vector(mode = "numeric", length = 0)
      
      concdat_yr <- as.numeric(gsub(".*-[0-9]*x[0-9]*x[0-9]*--([0-9][0-9][0-9][0-9]).txt",
                                   "\\1", concdat_files[i]))
      concdat_years <- c(concdat_years, concdat_yr)
    }
    
    # If there is at least one CONC.DAT file, provide a TRUE value for the concdat object
    if (number_concdat_years > 0) srfdat <- TRUE
    if (number_concdat_years == 0) srfdat <- FALSE
    
    # Determine the total number of CALPUFF.INP files that need to be made
    number_calpuff_input_files_to_make <- number_concdat_per_year * number_concdat_years
    
    # Generate a vector list of calpuff.inp keywords
    keywords <- c("GEODAT", "SRFDAT", "CLDDAT", "PRCDAT", "WTDAT", "METLST", "METDAT",
                  "PACDAT", "LCFILES", "NUSTA", "NOWSTA", "NM3D", "NIGF", "UPDAT",
                  "SEADAT", "M3DDAT", "IGFDAT", "DIADAT", "PRGDAT", "TSTPRT", "TSTOUT",
                  "TSTKIN", "TSTFRD", "TSTSLP", "DCSTGD")
    
    # Generate a vector list of the formatted replacements
    replacements <- c(geodat, srfdat, clddat, prcdat, wtdat, metlst, metdat,
                      pacdat, lcfiles, nusta, nowsta, nm3d, nigf, updat,
                      seadat, m3ddat, igfdat, diadat, prgdat, tstprt, tstout,
                      tstkin, tstfrd, tstslp, dcstgd)
    
    # Modify all parameters in working calmet.inp vector
    calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                         keyword = keywords,
                                         replacement = replacements)
    
    # Write the output to the same working calmet.inp file
    writeLines(calpuff_inp_working, con = calpuff_inp)
    
    # Read in the working calmet.inp file as a character vector
    calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
    
    # Determine which keywords contain the 'FALSE' indicator and prepare
    # those lines for writing with asterisks
    asterisk_lines <- grep("FALSE", calpuff_inp_working)
    
    # Take those FALSE value lines and disable them using asterisks
    calpuff_inp_working[asterisk_lines] <-
      gsub("!", "\\*", gsub("FALSE", "", calpuff_inp_working[asterisk_lines]))
    
    # Write the output to the same working calmet.inp file
    writeLines(calpuff_inp_working, con = calpuff_inp)
    
    # Read in the working calmet.inp file as a character vector
    calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
    
  # Remove the "calpuff_template.txt" file from the working folder
  file.remove("calpuff_template.txt")
  
}

