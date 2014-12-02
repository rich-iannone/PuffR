#' Retrieve the NCDC history data file
#' @description This function initiates a download of the NCDC surface station history file.
#' @param replace.file selecting 'yes' will overwrite history file if it exists in the working directory.
#' @export calmet_get_ncdc_history
#' @examples
#' \dontrun{
#' # Obtain the NCDC history file
#' calmet_get_ncdc_history()
#'}

calmet_get_ncdc_history <- function(replace.file = FALSE) {
  
  # Get hourly surface data history CSV from NOAA/NCDC FTP
  file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
  
  if (replace.file == TRUE) {
    repeat {
      
      try(download.file(file, "ish-history.csv", quiet = TRUE))
      
      if (file.info("ish-history.csv")$size > 0) { break }
      
    }
    
  } else { }
  
  # Check if file exists in working directory
  if (file.exists("ish-history.csv") &
        file.info("ish-history.csv")$size > 0 &
        replace.file == FALSE) { } else { 
          
          repeat {
            
            try(download.file(file, "ish-history.csv", quiet = TRUE))
            
            if (file.info("ish-history.csv")$size > 0) { break }
            
          }
          
        }
  
  # Read in the "ish-history" CSV file
  st <- read.csv("ish-history.csv")
  
}
