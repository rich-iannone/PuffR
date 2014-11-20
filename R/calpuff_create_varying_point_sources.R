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
  UTM_zone <- gsub(" ", "", geo_dat_lines[grep("UTM", geo_dat_lines) + 1])
  
  # Use 'domain_dimensions' to get the first item from a vector list of SURF.DAT files
  surf_dat_file <- list.files(pattern = paste0("surf--.*?-", domain_dimensions, ".*"))[1]
  
  # Obtain text lines of SURF.DAT file as a vector object
  surf_dat_lines <- readLines(surf_dat_file, warn = FALSE)
  
  # Obtain time zone text from 'surf_dat_lines'
  time_zone <- gsub(" ", "", surf_dat_lines[grep("UTC([+|-])", surf_dat_lines)])
  
  # If data frame provided, read in that data frame object
  if (!is.null(df_input)){
    
    # Assign 'df_input' to 'point_sources_df'
    point_sources_df <- df_input
    
    # Check that each required column is present
    all_columns_present <- ifelse(all(c("src_name", "date_time", "x", "y",
                                        "stk_height", "stk_diam", "stk_base_elev",
                                        "bldg_downwash", "user_flag") %in% colnames(point_sources_df)),
                                  TRUE, FALSE)
    
    # Determine the class of 'src_name' column, tranforming to 'character'
    if (class(point_sources_df$src_name) == "character"){
      NULL
    } else if (class(point_sources_df$src_name) == "factor"){
      point_sources_df$src_name <- as.character(point_sources_df$src_name)
    } else if (class(point_sources_df$src_name) == "numeric"){
      point_sources_df$src_name <- as.character(point_sources_df$src_name)
    }

    # Determine the class of 'date_time' column, tranforming to 'POSIXct'
    if (class(point_sources_df$date_time)[1] == "POSIXct"){
      NULL
    } else if (class(point_sources_df$date_time) == "factor"){
      point_sources_df$date_time <- as.POSIXct(as.numeric(as.character(point_sources_df[,2])),
                                              origin = "1970-01-01", tz = "GMT")  
    } else if (class(point_sources_df$date_time) == "numeric"){
      point_sources_df$date_time <- as.POSIXct(point_sources_df[,2],
                                              origin = "1970-01-01", tz = "GMT") 
    }
    
    # Change 'character'/'factor' classes to numeric class
    for (i in 3:9){
      if (class(point_sources_df[,i]) == "character"){
        point_sources_df[,i] <- as.numeric(point_sources_df[,i])
      } else if (class(point_sources_df[,i]) == "factor"){
        point_sources_df[,i] <- as.numeric(as.character(point_sources_df[,i]))
      }
    }
        
  }
  
  # Get beginning date and time
  beginning_date_time <- min(point_sources_df$date_time)
  
  # Get ending date and time
  
  
  # Construct header lines for file
  header_1 <- paste0("PTEMARB.DAT     2.1             ",
                     "Comments, times with seconds, time zone, coord info")
  header_2 <- "1"
  header_3 <- "Produced by PuffR !Do not edit by hand!"
  header_4 <- "UTM"
  header_5 <- UTM_zone
  header_6 <- "WGS-84"
  header_7 <- "  KM"
  header_8 <- time_zone
  header_9 <- "1988  11   0  0000  1988  11  24 0000"
  
  
}
