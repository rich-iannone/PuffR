#' Create CALPUFF input file with point sources that vary in location and time
#' @description Create CALPUFF input file with point sources that vary in location and time.
#' @param CSV_input a path to a CSV file containing hourly point source data.
#' @param df_input a data frame containing hourly point source data.
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @export calpuff_create_varying_point_sources

calpuff_create_varying_point_sources <- function(CSV_input = NULL,
                                                 df_input = NULL,
                                                 src_name,
                                                 species_name){

  # Obtain domain dimensions from CALPUFF output files
  calpuff_out_files <- list.files(pattern = "calpuff_out--concdat--.*")
  domain_dimensions <- unique(gsub("^calpuff_out--concdat--.*?-([x0-9]*).*", "\\1", calpuff_out_files))[1]
  
  # Use 'domain_dimensions' to get the first item from a vector list of GEO.DAT files
  geo_dat_file <- list.files(pattern = paste0("geo--.*?-", domain_dimensions, ".*"))[1]
  
  # Obtain text lines of GEO.DAT file as a vector object
  geo_dat_lines <- readLines(geo_dat_file, warn = FALSE)
  
  # Obtain UTM zone and hemisphere text from 'geo_dat_lines'
  geo_dat_UTM_line <- gsub(" ", "", geo_dat_lines[grep("UTM", geo_dat_lines) + 1])
  
  # Use 'domain_dimensions' to get the first item from a vector list of SURF.DAT files
  surf_dat_file <- list.files(pattern = paste0("surf--.*?-", domain_dimensions, ".*"))[1]

  # Obtain text lines of SURF.DAT file as a vector object
  surf_dat_lines <- readLines(surf_dat_file, warn = FALSE)
  
  
  # Construct header lines for file
  header_1 <- paste0("PTEMARB.DAT     1.54a           ",
                     "Augmented 5.4 format with Map Projection, DATUM, Time Zone")
  header_2 <- "UTM"
  header_3 <- geo_dat_UTM_line
  header_4 <- "WGS-84"
  
  
}
