##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
##                                                                      ##
##  R project to generate useful tools for dispersion modelling with    ##
##  the CALPUFF system                                                  ##
##                                                                      ##
##  Rich Iannone, 2013                                                  ##
##                                                                      ##
##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##

# Load in necessary packages
install.packages("sp")
library("sp")
install.packages("rgdal")
library("rgdal")
install.packages("shapefiles")
library("shapefiles")
install.packages("raster")
library("raster")
install.packages("plyr")
library("plyr")
install.packages("spdep")
library("spdep")
install.packages("fields")
library("fields")
install.packages("MBA")
library("MBA")
install.packages("stringr")
library("stringr")

# Add the CSV data file on EPSG codes
EPSG_proj4 <- read.csv("EPSG_codes.csv", header = TRUE)

## Function start #### define.calmet.domain ########################################
define.calmet.domain <- function(lat.dec.deg = NULL,
                                 long.dec.deg = NULL,
                                 lat.long.grid.loc = 1,
                                 EPSG_code = NULL,
                                 projection = "UTM",
                                 datum = "WGS84",
                                 ellipse = "WGS84",
                                 units = "m",
                                 cell.resolution.m = 250,
                                 domain.width.m = NULL,
                                 domain.height.m = NULL) {

require(rgdal)
require(plyr)
  
#lat.dec.deg <- 34.050184
#long.dec.deg <- -118.253959

#EPSG_code <- 32611

# projection <- "UTM"
# datum <- "WGS84"
# ellipse <- "WGS84"
# units <- "m"

# Where is this point located on the grid?
# Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
#lat.long.grid.loc <- 1
  
# what is the the cell resolution (square cells) required (in meters)?
#cell.resolution.m <- 250

# What is the width and the height of the met domain in meters? Extents will be generated based on the
# location of chosen point.
#domain.width.m <- 100000
#domain.height.m <- 100000

lat_long_dec_deg <- cbind(long.dec.deg, lat.dec.deg)
UTM_zone <- (floor((long.dec.deg + 180)/6) %% 60) + 1

# Generate a PROJ.4 string from an acceptable EPSG code
# Include a dataframe that contains all acceptable EPSG codes for filtering
# Information on this page: http://cicero.azavea.com/docs/epsg_codes.html

EPSG_string <- subset(EPSG_proj4, EPSG_proj4$code == EPSG_code)
EPSG_string <- EPSG_string$proj4_string

# NOTE: with the above-entered info, should be able to obtain one EPSG code from a lookup data frame
proj_string <- paste("+init=epsg:", EPSG_code, sep = "")
proj_string_longlat <- c("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Project as UTM coordinate, round to nearest 50 m using round_any function ('plyr' package)
UTM_location <- project(lat_long_dec_deg, proj_string)
UTM_location <- round_any(UTM_location, 50, round)

# Do these length and width values accomodate an integer number of cells of the specified resolution?
# These checks will be later part of a function in setting domain width and height
is_number_cells_across_x_an_int <- ifelse(domain.width.m %% cell.resolution.m != 0, FALSE, TRUE)
is_number_cells_across_y_an_int <- ifelse(domain.height.m %% cell.resolution.m != 0, FALSE, TRUE)

number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                domain.width.m/cell.resolution.m, NULL)

number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                domain.height.m/cell.resolution.m, NULL)

total_cells <- number_cells_across_x * number_cells_across_y

# Get extents of UTM grid (left, right, bottom, top)
left_UTM <- if(lat.long.grid.loc == 1) {
          UTM_location[1,1] - (0.5 * domain.width.m)
          } else if (lat.long.grid.loc == 2) {
          UTM_location[1,1]
          } else if (lat.long.grid.loc == 3) {
          UTM_location[1,1] - domain.width.m
          } else if (lat.long.grid.loc == 4) {
            UTM_location[1,1]
          } else if (lat.long.grid.loc == 5) {
            UTM_location[1,1] - domain.width.m
          } else {
          NULL
          }

right_UTM <- if(lat.long.grid.loc == 1) {
          UTM_location[1,1] + (0.5 * domain.width.m)
          } else if (lat.long.grid.loc == 2) {
          UTM_location[1,1] + domain.width.m
          } else if (lat.long.grid.loc == 3) {
          UTM_location[1,1]
          } else if (lat.long.grid.loc == 4) {
          UTM_location[1,1] + domain.width.m
          } else if (lat.long.grid.loc == 5) {
          UTM_location[1,1]
          } else {
          NULL
          }

bottom_UTM <- if(lat.long.grid.loc == 1) {
          UTM_location[1,2] - (0.5 * domain.height.m)
          } else if (lat.long.grid.loc == 2) {
          UTM_location[1,2]
          } else if (lat.long.grid.loc == 3) {
          UTM_location[1,2]
          } else if (lat.long.grid.loc == 4) {
          UTM_location[1,2] - domain.height.m
          } else if (lat.long.grid.loc == 5) {
          UTM_location[1,2] - domain.height.m
          } else {
          NULL
          }

top_UTM <- if(lat.long.grid.loc == 1) {
        UTM_location[1,2] + (0.5 * domain.height.m)
        } else if (lat.long.grid.loc == 2) {
        UTM_location[1,2] + domain.height.m
        } else if (lat.long.grid.loc == 3) {
        UTM_location[1,2] + domain.height.m
        } else if (lat.long.grid.loc == 4) {
        UTM_location[1,2]
        } else if (lat.long.grid.loc == 5) {
        UTM_location[1,2]
        } else {
        NULL
        }

## Need to obtain spatial points in lat/lon for LL, LR, UL, UR
LL_LR_UL_UR_m <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))

LL_LR_UL_UR_UTM_m <- SpatialPoints(as.matrix(LL_LR_UL_UR_m),
                               proj4string = CRS(paste("+init=epsg:", EPSG_code, sep = "")))

LL_LR_UL_UR_UTM_longlat <- spTransform(LL_LR_UL_UR_UTM_m, CRS("+proj=longlat +ellps=GRS80"))

define.calmet.domain.out <- mat.or.vec(7, 1)

# Return vector with: [1] - [4] bounding box lat/long coordinates (W, E, N, S),
# [5] number of cells in x direction, [6] number of cells in y direction,
# [7] total number of cells
define.calmet.domain.out[1] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,1]
define.calmet.domain.out[2] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,2]
define.calmet.domain.out[3] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,2]
define.calmet.domain.out[4] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,1]
define.calmet.domain.out[5] <- number_cells_across_x
define.calmet.domain.out[6] <- number_cells_across_y
define.calmet.domain.out[7] <- total_cells

write.table(define.calmet.domain.out, file = "define.calmet.domain.out",
            col.names = FALSE, row.names = FALSE)

print(define.calmet.domain.out)
}
## Function end  #### define.calmet.domain ########################################



## Function start #### get.ncdc.history ###########################################
get.ncdc.history <- function(replace.file = FALSE) {

# Check if file exists in working directory
  if (file.exists("ish-history.csv") &
      file.info("ish-history.csv")$size > 0 &
      replace.file == FALSE) {
  stop("The 'ish-history.csv' file is available. To download again, use 'replace.file = TRUE'")
  } else { }
  
# Get hourly surface data history CSV from NOAA/NCDC FTP
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
repeat {
  try(download.file(file, "ish-history.csv", quiet = TRUE))
  if (file.info("ish-history.csv")$size > 0) { break }
  }

# Read in the "ish-history" CSV file
st <- read.csv("ish-history.csv")
}
## Function end  #### get.ncdc.history ############################################


## Function start #### ncdc.station.data ###############################################
ncdc.station.data <- function(startyear = NULL,
                               endyear = NULL,
                               use.calmet.bounds = TRUE,
                               manual.bounds = NULL) {

# Initially set use of manual bounds to FALSE
use.manual.bounds <- FALSE

# Check whether 'startyear' and 'endyear' are both provided
  if (is.null(startyear) | is.null(endyear)) {
    stop("Please enter starting and ending years for surface station data")
  } else { }

# Check whether 'startyear' and 'endyear' are both numeric
  if (!is.numeric(startyear) | !is.numeric(endyear)) {
    stop("Please enter numeric values for the starting and ending years")
  } else { }

# Check whether 'startyear' and 'endyear' are in the correct order
  if (startyear > endyear) {
    stop("Please enter the starting and ending years in the correct order")
  } else { }

# Check whether 'staryear' and 'endyear' are within set bounds (1950 to current year)
  if (startyear < 1892 | endyear < 1892 | startyear > year(Sys.Date()) | endyear > year(Sys.Date())) {
    stop("Please enter the starting and ending years in the correct order")
  } else { }

# Check whether the CALMET domain bounds (from the function 'define.calmet.domain') are available
  if (!exists("latlong_bbox_west") |
      !exists("latlong_bbox_east") |  
      !exists("latlong_bbox_north") |
      !exists("latlong_bbox_south")) {
    stop("The CALMET domain hasn't been defined. Please define a domain enter bounds manually.")
  } else { }

# Check whether input to for 'manual.bounds' was made, and, if so, check whether manual bounds
# are entered correctly
  if (!is.null(manual.bounds)) {
    if (!is.numeric(manual.bounds[1]) |
          !is.numeric(manual.bounds[2]) |  
          !is.numeric(manual.bounds[3]) |
          !is.numeric(manual.bounds[4]) |
          length(manual.bounds) != 4) {
      stop("Please define manual bounds as numeric vector in the order: W, E, N, S")
    } else { use.manual.bounds <- TRUE }
  }

# Define time parameters
NOAA_start_year <- startyear
NOAA_end_year <- endyear

# Get hourly surface data history CSV from NOAA/NCDC FTP
get.ncdc.history()

# Read in the 'ish-history.csv' file
st <- read.csv("ish-history.csv")

# Get formatted list of station names and elevations
names(st)[c(3, 10)] <- c("NAME", "ELEV")
st <- st[, -5]

# Previous code made subset of station data for those just in US and Canada:
# st.can <- st[st$CTRY == "CN", ]
# st.us <- st[st$CTRY == "US", ]
# st <- merge(st.can, st.us, all = TRUE)

# Reintroduce the decimals in the latitude, longitude, and elevation
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10

# Recompose the years from the data file
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

# Generate a list based on the domain location, also ignoring stations without
# beginning years reported


if (use_manual_bounds == TRUE) {
domain.list <- subset(st, st$LON >= manual.bounds[1] & 
                          st$LON <= manual.bounds[2] &
                          st$LAT >= manual.bounds[3] &
                          st$LAT <= manual.bounds[4] &
                          BEGIN <= NOAA_start_year &
                          END >= NOAA_end_year)
} else if (use.calmet.bounds == TRUE) { 
domain.list <- subset(st, st$LON >= define.calmet.domain.out[1] & 
                          st$LON <= define.calmet.domain.out[2] &
                          st$LAT <= define.calmet.domain.out[3] &
                          st$LAT >= define.calmet.domain.out[4] &
                          BEGIN <= NOAA_start_year &
                          END >= NOAA_end_year)
}

# Initialize data frame for file status reporting
outputs <- as.data.frame(matrix(NA, dim(domain.list)[1], 2))
names(outputs) <- c("FILE", "STATUS")

# Download the gzip-compressed data files for the years specified
# Provide information on the number of records in data file retrieved 
for (y in NOAA_start_year:NOAA_end_year) {
  y.domain.list <- domain.list[domain.list$BEGIN <= y & domain.list$END >= y, ]
  for (s in 1:dim(y.domain.list)[1]) {
    outputs[s, 1] <- paste(sprintf("%06d", y.domain.list[s,1]),
                           "-", sprintf("%05d", y.domain.list[s,2]),
                           "-", y, ".gz", sep = "")
    system(paste("curl -O ftp://ftp3.ncdc.noaa.gov/pub/data/noaa/", y,
                     "/", outputs[s, 1], sep = ""))
    outputs[s, 2] <- ifelse(file.exists(outputs[s, 1]) == "TRUE", 'available', 'missing')
  }
}

# Generate report of stations and file transfers
file_report <- cbind(y.domain.list, outputs)
row.names(file_report) <- 1:nrow(file_report)

# Extract all downloaded data files
system("gunzip *.gz", intern = FALSE, ignore.stderr = TRUE)

# Read data from files
# Specific focus here on the fixed width portions ('Mandatory Data Section') of each file
files <- list.files(pattern = "^[0-9]*-[0-9]*-[0-9]*$")
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1,
                   5, 1, 1, 1, 6, 1, 1, 1, 5, 1,
                   5, 1, 5, 1)
stations <- as.data.frame(matrix(NA, length(files), 6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")

for (i in 1:length(files)) {
    # Read data from mandatory data section of each file, which is a fixed-width string
    data <- read.fwf(files[i], column.widths)
    data <- data[, c(2:8, 10:11, 13, 16, 19, 21, 29, 31, 33)]
    names(data) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN", "LAT", "LONG",
                     "ELEV", "WIND.DIR", "WIND.SPD", "CEIL.HGT", "TEMP", "DEW.POINT",
                     "ATM.PRES")
    
    # Recompose data and use consistent missing indicators of 9999 for missing data
    data$LAT <- data$LAT/1000
    data$LONG <- data$LONG/1000
    data$WIND.DIR <- ifelse(data$WIND.DIR == 999, 999, data$WIND.DIR)
    data$WIND.SPD <- ifelse(data$WIND.SPD > 100, 999.9, data$WIND.SPD/10)
    data$TEMP <-  ifelse(data$TEMP > 100, 9999, round((data$TEMP/10) + 273.2, 1))
    data$DEW.POINT <- ifelse(data$DEW.POINT > 100, 999.9, data$DEW.POINT/10)
    data$ATM.PRES <- ifelse(data$ATM.PRES > 2000, 999.9, data$ATM.PRES/10)
    data$CEIL.HGT <- ifelse(data$CEIL.HGT == 99999, 999.9, round(data$CEIL.HGT*3.28084/100, 0))
    
    # Read data from additional data section of each file
    # Additional data is of variable length and may not exist in every line of every file
    additional.data <- as.data.frame(scan(files[i], what = 'character', sep = "\n"))
    colnames(additional.data) <- c("string")
    number_of_add_lines <- sum(str_detect(additional.data$string, "ADD"), na.rm = TRUE)
    percentage_of_add_lines <- (number_of_add_lines/length(additional.data$string)) * 100
    
    # precipitation: AA[1-2]
    number_of_precip_lines <- sum(str_detect(additional.data$string, "AA1"), na.rm = TRUE)
    percentage_of_precip_lines <- (number_of_precip_lines/length(additional.data$string)) * 100
    
    if (number_of_precip_lines > 0) {
      number_of_precip_lines2 <- ifelse(number_of_precip_lines > 0,
                                        sum(str_detect(additional.data$string,
                                                       "AA2"), na.rm = TRUE), 0)
      percentage_of_precip_lines2 <- ifelse(number_of_precip_lines2 > 0,
                                            (number_of_precip_lines2/
                                               length(additional.data$string)) * 100, 0)
      
      AA1_precip_period_in_hours <- unlist(str_extract_all(additional.data$string, "AA1[0-9][0-9]"))
      AA1_precip_period_in_hours <- str_replace_all(AA1_precip_period_in_hours,
                                                    "AA1([0-9][0-9])", "\\1")
      AA1_precip_period_in_hours <- as.numeric(AA1_precip_period_in_hours)
      
      AA1_precip_depth_in_mm <- unlist(str_extract_all(additional.data$string,
                                                       "AA1[0-9][0-9][0-9][0-9][0-9][0-9]"))
      AA1_precip_depth_in_mm <- str_replace_all(AA1_precip_depth_in_mm,
                                                "AA1[0-9][0-9]([0-9][0-9][0-9][0-9])", "\\1")
      AA1_precip_depth_in_mm <- as.numeric(AA1_precip_depth_in_mm)/10
      
      # don't have the means yet to generate a PRECIP.DAT file, so, set to 9999 for now
      precip_code <- rep(999, length(additional.data$string))
      additional.data$PRECIP.CODE <- precip_code
    } 
    
    if (number_of_precip_lines == 0) {
      # put in vector of 999 in PRECIP.CODE column of data frame
      additional.data$PRECIP.CODE <- rep(999, length(additional.data$string))
    }

    # relative humidity: RH[1-3]
    number_of_RH_lines <- sum(str_detect(additional.data$string, "RH1"), na.rm = TRUE)
    percentage_of_RH_lines <- (number_of_RH_lines/length(additional.data$string)) * 100  
    if (number_of_RH_lines > 0) {
      number_of_RH_lines2 <- ifelse(number_of_RH_lines > 0,
                                    sum(str_detect(additional.data$string, "AA2"),
                                        na.rm = TRUE), 0)
      percentage_of_RH_lines2 <- ifelse(number_of_RH_lines2 > 0,
                                        (number_of_RH_lines2/
                                           length(additional.data$string)) * 100, 0)
      RH1_RH_in_percent <- unlist(str_extract_all(additional.data$string, "AA1[0-9][0-9]"))
      RH1_RH_in_percent <- str_replace_all(RH1_RH_in_percent,
                                           "AA1([0-9][0-9])", "\\1")
      RH1_RH_in_percent <- as.numeric(RH1_RH_in_percent)
      additional.data$RH <- RH1_RH_in_percent
    }
    
    if (number_of_RH_lines == 0) {
      additional.data$RH <- rep(999.0, length(additional.data$string))
    }
    
    # Remove the string portion of the 'additional data' data frame
    additional.data$string <- NULL
    
    # Column bind the 'data' and 'additional data' data frame
    data <- cbind(data, additional.data)
    
    # Write CSV file for each station, combining data elements from the mandatory data
    # section and the additional data section
    write.csv(data, file = paste(files[i], ".csv", sep = ""), row.names = FALSE)
    
    # Create a data frame with summary data for each station
    stations[i, 1:3] <- data[1, 1:3]
    stations[i, 4:6] <- data[1, 8:10]
}

# Write the station data to a CSV file
write.csv(stations, file = "stations.csv", row.names = FALSE)

# Remove objects from the workspace
rm(use.manual.bounds, domain.list, y.domain.list, outputs, files, file_report,
   number_of_add_lines, percentage_of_add_lines,
   percentage_of_precip_lines, percentage_of_precip_lines2,
   number_of_precip_lines, number_of_precip_lines2, 
   AA1_precip_period_in_hours, AA1_precip_depth_in_mm, precip_code,
   number_of_RH_lines, percentage_of_RH_lines, number_of_RH_lines2,
   percentage_of_RH_lines2, RH1_RH_in_percent,
   data, additional.data)

}
## End of function #### ncdc.station.data ###########################################


## Function start #### plot.calmet.domain ###########################################

# Load in a raster file for Canada 
canada_raster <- raster("CAN_alt.gri")

# Crop raster to domain + a margin of 0.2 degrees
extent_in_lat_long <- extent(coordinates(LL_LR_UL_UR_UTM_longlat)[1,1],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[2,1],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[1,2],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[3,2])

domain_raster <- crop(canada_raster, extent_in_lat_long)

domain_raster.spdf <- as(domain_raster, "SpatialPixelsDataFrame")
r.df <- as.data.frame(domain_raster.spdf)
head(r.df)

plot_domain <- ggplot(r.df, aes(x = x, y = y)) +
               geom_rect(xmin = define.calmet.domain.out[1], xmax = define.calmet.domain.out[2],
                         ymax = define.calmet.domain.out[3], ymin = define.calmet.domain.out[4]) +
               geom_tile(aes(fill = CAN_alt)) +
               scale_fill_gradient(low = "green", high = "white") +
               geom_point(data = stations,
                          aes(x = LONG, y = LAT,
                              colour = "#D55E00", size = 3)) +
               geom_text(data = stations,
                         aes(x = LONG+.01, y = LAT, label = USAFID,
                         colour = "#56B4E9",
                         hjust = 0, vjust = 0)) +
               coord_equal() +
               xlim(define.calmet.domain.out[1], define.calmet.domain.out[2]) +
               ylim(define.calmet.domain.out[4], define.calmet.domain.out[3]) +
               theme(legend.position = "none") +
               labs(x = "Longitude") +
               labs(y = "Latitude") +
               labs(title = "Plot of Surface Stations in Meteorological Domain")
## End of function ### plot.calmet.domain ###########################################

# Get stations to be included in SURF.DAT
# Need to ensure that files corresponding to the proper years are included
# need list of stations (list of USAF-WBAN strings as ID)

# test data for 'station.select' function
id <- c("720046-99999", "722880-23152")

## Function start #### station.select ###############################################
station.select <- function(id) {
  # id is a list of USAF/WBAN IDs, separated by a dash
  # id <- c("720046-99999", "722880-23152")
  # test whether these files are in the 'stations' data frame
  # and further that these files are in the working directory
  station_years <- c(NOAA_start_year:NOAA_end_year)
  number_of_stations <- length(id)
  available_synthetic_id <- as.data.frame(paste(stations$USAFID, stations$WBAN, sep = "-"))
  colnames(available_synthetic_id) <- c("USAF_WBAN")
  available_station_years <- as.data.frame(paste(stations$YR))
  selected_synthetic_id <- as.data.frame(cbind(id), row.names = NULL)
  colnames(selected_synthetic_id) <- c("id")
  for (i in 1:number_of_stations) {
    selected_synthetic_id$available[i] <- id[i] %in% available_synthetic_id$USAF_WBAN
  }
  for (i in 1:number_of_stations) {
    if(selected_synthetic_id$available[i] == FALSE) {
      print("Some stations missing.")
      # download missing station data from NOAA/NCDC
    }
  }
  # Construct list of files from which data will be extracted
  for (i in 1:nrow(selected_synthetic_id)) {
    for (j in 1:length(station_years)) {
  selected_synthetic_id$CSV[i] <- paste(selected_synthetic_id$id[i], "-", station_years[j],
                                        ".csv", sep = "")
    }
  }
  selected_synthetic_id
}
## Function end   #### station.select ###############################################

## Function start #### surf.dat.generate ###############################################
surf.dat.generate <- function(startyear = NOAA_start_year,
                              endyear = NOAA_end_year,
                              outputfile = "surf.dat") {

require(lubridate)

# Define the start and end times and determine number of hours in each year
start_time <- ISOdatetime(startyear, 1, 1, hour = 0, min = 0, sec = 0, tz = "GMT")
end_time <- ISOdatetime(endyear, 12, 31, hour = 24, min = 0, sec = 0, tz = "GMT")
time_difference <- difftime(end_time, start_time, units='hours')
total_hours <- time_difference[[1]]

# Create the hourly time series as a list of POSIXlt time objects
time_series <- as.list(c(1:total_hours))
for (i in 1:total_hours) {
time_series[[i]] <- start_time + (3600 * (i - 1))
}

# Use selected_synthetic_id to extract data from specified stations
station_data_frames <- as.list(c(1:nrow(selected_synthetic_id)))

for (i in 1:nrow(selected_synthetic_id)) {
station_data_frames[[i]] <- list(read.csv(paste("/Users/riannone/Dropbox/R Projects/PuffR/",
                            selected_synthetic_id$CSV[i],
                            sep = ''), header = TRUE))
}

# Write out the SURF.DAT file
#
# The following parameters are used:
#
# Name      Description
# --------  -----------
# IYR       Year of data
# IJUL      Julian day
# IHR       Hour (00-23 LST)
# WS        Wind Speed (m/s)
# WD        Wind Direction (degrees)
# ICEIL     Ceiling Height (hundreds of feet)
# ICC       Opaque Sky Cover (tenths)
# TEMPK     Air temperature (K)
# IRH       Relative humidity (percent)
# PRES      Station Pressure
# IPCODE    Precipitation code
#           (0=no precipitation, 1-18=liquid precipitation, 19-45=frozen precipitation)

# Construct the file header records of the SURF.DAT file

# Initialize file for writing
cat("", file = paste(outputfile))

# Add line 1 to file header (dataset name [SURF.DAT], dataset version [2.1], dataset message field)
cat("SURF.DAT        2.1             Hour Start and End Times with Seconds",
    file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 2 to file header (number of comment lines to follow)
cat("1",
    file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 3 to file header (single comment line)
cat("Produced using R",
    file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 4 to file header (map projection [NONE])
cat("NONE",
    file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 5 to file header (time zone)
cat("UTC-0800",
    file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 6 to file header (Beginning and end times for file, number of met stations)
cat(year(time_series[[1]]),
      "  ",
      yday(time_series[[1]]),
      "  ",
      hour(time_series[[1]]),
      "  ",
      year(time_series[[total_hours]] + 3600),
      "  ",
      yday(time_series[[total_hours]] + 3600),
      "  ",
      hour(time_series[[total_hours]] + 3600),
      "  ",
      nrow(selected_synthetic_id),
      file = paste(outputfile), sep = '', append = TRUE)

cat("", file = paste(outputfile), sep = "\n", append = TRUE)

# Add line 7- to file (5-digit identifiers for met stations used in file)
for (i in 1:length(station_data_frames)) {
  cat(paste(as.character(i),
            as.character(i),
            as.character(i),
            as.character(i),
            as.character(i), sep = ''), file = paste(outputfile), sep = "\n", append = TRUE)
  }

# Construct the body of the SURF.DAT file using a nested loops that provide grouped time interval
# headers with ordered surface station parameters

for (i in 1:total_hours) {
  cat(year(time_series[[i]]),
      "  ",
      yday(time_series[[i]]),
      "  ",
      hour(time_series[[i]]),
      "  ",
      year(time_series[[i]] + 3600),
      "  ", 
      yday(time_series[[i]] + 3600),
      "  ",
      hour(time_series[[i]] + 3600), file = paste(outputfile), append = TRUE)
  cat("", file = paste(outputfile), sep = "\n", append = TRUE)
  for (j in 1:length(station_data_frames)) {
    cat("  ",
        # Wind speed, m/s (WS)
        station_data_frames[[j]][[1]]$WIND.SPD[i],
        "  ",
        # Wind direction, degrees (WD)
        station_data_frames[[j]][[1]]$WIND.DIR[i],
        "  ",
        # Ceiling height, hundreds of feet (ICEIL)
        station_data_frames[[j]][[1]]$CEIL.HGT[i],
        "  ",
        # Opaque sky cover, tenths (ICC)
        # (still to prepare)
        "  ",
        # Air temperature, K (TEMPK)
        station_data_frames[[j]][[1]]$TEMP[i],
        "  ",
        # Relative humidity, % (IRH)
        station_data_frames[[j]][[1]]$RH[i],
        "  ",
        # Station pressure, mb (PRES)
        station_data_frames[[j]][[1]]$ATM.PRES[i],
        "  ",
        # Precipitation code (IPCODE) 
        #   0 = no precipitation
        #   1-18 = liquid precipitation
        #   19-45 = frozen precipitation
        station_data_frames[[j]][[1]]$PRECIP.CODE[i], file = paste(outputfile), append = TRUE)
    
    cat("", file = paste(outputfile), sep = "\n", append = TRUE) } }
#
#
}
## End of function #### surf.dat.generate ##############################################


# Experiments with making grids with the 'fields' package, this is unfinished
#
# grid.list <- list(x = seq((left_UTM + (0.5 * cell_resolution_m)), 
#                           (right_UTM - (0.5 * cell_resolution_m)), by = cell_resolution_m),
#                   y = seq((bottom_UTM + (0.5 * cell_resolution_m)), 
#                           (top_UTM - (0.5 * cell_resolution_m)), by = cell_resolution_m))
# xg <- make.surface.grid(grid.list)