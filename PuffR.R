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

# Start with information on location for CALMET domain

# Let's start with lat/long in decimal degrees, convert to UTM, then develop a grid

lat_dec_deg <- 49.062145
long_dec_deg <- -123.128404
lat_long_dec_deg <- cbind(long_dec_deg, lat_dec_deg)

# Generate a PROJ.4 string from an acceptable EPSG code
# Include a dataframe that contains all acceptable EPSG codes for filtering
# Information on this page: http://cicero.azavea.com/docs/epsg_codes.html

# Use the following code for testing purposes: 32610 (WGS 84 / UTM zone 10N)
# Should be the PROJ.4 string:
# +init=epsg:32610 +proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 

projection <- c("utm")
UTM_zone <- (floor((long_dec_deg + 180)/6) %% 60) + 1
datum <- c("WGS84")
units <- c("m")

# NOTE: with the above-entered info, should be able to obtain one EPSG code from a lookup data frame
EPSG_code <- 32610
proj_string <- c("+init=epsg:32610 +proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj_string_longlat <- c("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Project as UTM coordinate, round to nearest 50 m using rdound_any function ('plyr' package)
UTM_location <- project(lat_long_dec_deg, proj_string)
UTM_location <- round_any(UTM_location, 50, round)

# Where is this point located on the grid?
# Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
lat_long_grid_loc <- 1

# what is the the cell resolution (square cells) required (in meters)?
cell_resolution_m <- 250

# What is the width and the height of the met domain in meters? Extents will be generated based on the
# location of chosen point.
met_domain_width_m <- 50000
met_domain_height_m <- 50000

# Do these length and width values accomodate an integer number of cells of the specified resolution?
# These checks will be later part of a function in setting domain width and height
is_number_cells_across_x_an_int <- ifelse(width_met_domain_m %% cell_resolution_m != 0, FALSE, TRUE)
is_number_cells_across_y_an_int <- ifelse(width_met_domain_m %% cell_resolution_m != 0, FALSE, TRUE)

number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                width_met_domain_m/cell_resolution_m, NULL)

number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                height_met_domain_m/cell_resolution_m, NULL)

total_cells <- number_cells_across_x * number_cells_across_y

# Get extents of UTM grid (left, right, bottom, top)
left_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,1] - (0.5 * met_domain_width_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,1]
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,1] - met_domain_width_m
          } else if (lat_long_grid_loc == 4) {
            UTM_location[1,1]
          } else if (lat_long_grid_loc == 5) {
            UTM_location[1,1] - met_domain_width_m
          } else {
          NULL
          }

right_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,1] + (0.5 * met_domain_width_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,1] + met_domain_width_m
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,1]
          } else if (lat_long_grid_loc == 4) {
          UTM_location[1,1] + met_domain_width_m
          } else if (lat_long_grid_loc == 5) {
          UTM_location[1,1]
          } else {
          NULL
          }

bottom_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,2] - (0.5 * met_domain_height_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,2]
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,2]
          } else if (lat_long_grid_loc == 4) {
          UTM_location[1,2] - met_domain_height_m
          } else if (lat_long_grid_loc == 5) {
          UTM_location[1,2] - met_domain_height_m
          } else {
          NULL
          }

top_UTM <- if(lat_long_grid_loc == 1) {
        UTM_location[1,2] + (0.5 * met_domain_height_m)
        } else if (lat_long_grid_loc == 2) {
        UTM_location[1,2] + met_domain_height_m
        } else if (lat_long_grid_loc == 3) {
        UTM_location[1,2] + met_domain_height_m
        } else if (lat_long_grid_loc == 4) {
        UTM_location[1,2]
        } else if (lat_long_grid_loc == 5) {
        UTM_location[1,2]
        } else {
        NULL
        }

## Need to obtain spatial points in lat/lon for LL, LR, UL, UR

LL_LR_UL_UR_m <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))

LL_LR_UL_UR_UTM_m <- SpatialPoints(as.matrix(LL_LR_UL_UR_m),
                               proj4string=CRS("+init=epsg:32610"))

LL_LR_UL_UR_UTM_longlat <- spTransform(LL_LR_UL_UR_UTM_m, CRS("+proj=longlat +ellps=GRS80"))

latlong_bbox_west <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,1]
latlong_bbox_east <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,2]
latlong_bbox_north <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,2]
latlong_bbox_south <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,1]


# Experiments with making grids with the 'fields' package, this is unfinished

grid.list <- list(x = seq((left_UTM + (0.5 * cell_resolution_m)), 
                          (right_UTM - (0.5 * cell_resolution_m)), by = cell_resolution_m),
                  y = seq((bottom_UTM + (0.5 * cell_resolution_m)), 
                          (top_UTM - (0.5 * cell_resolution_m)), by = cell_resolution_m))
xg <- make.surface.grid(grid.list)



##----- SURF.DAT ---- Surface data

# This section will focus on:
# (1) obtaining station data from NOAA/NCDC
# (2) inputting station data manually from a CSV

#
# (1) NOAA/NCDC Integrated Surface Data
#

# time parameters
NOAA_start_year <- 2010
NOAA_end_year <- 2010

# Get hourly surface data history CSV from NOAA/NCDC FTP
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
repeat {
  try(download.file(file, "ish-history.csv",
                    quiet = TRUE))
  if (file.info("ish-history.csv")$size >
        0) {
    break
  }
}

# Read in the "ish-history" CSV file
st <- read.csv("ish-history.csv")

# Get formatted list of stations from Canada ("CN") and USA ("US")
names(st)[c(3, 10)] <- c("NAME", "ELEV")
st <- st[, -5]
st.can <- st[st$CTRY == "CN", ]
st.us <- st[st$CTRY == "US", ]
st <- merge(st.can, st.us, all = TRUE)

# Reintroduce the decimals in the latitude, longitude, and elevation
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10

# Recompose the years from the data file
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

# Generate a list based on the domain location, also ignoring stations without
# beginning years reported
domain.list <- st[st$LON >= latlong_bbox_west & 
                  st$LON <= latlong_bbox_east &
                  st$LAT >= latlong_bbox_south &
                  st$LAT <= latlong_bbox_north &
                    (st$BEGIN <= NOAA_start_year &
                                          st$END >= NOAA_end_year & !is.na(st$BEGIN)), ]

# Initialize data frame for file status reporting
outputs <- as.data.frame(matrix(NA, dim(domain.list)[1], 2))
names(outputs) <- c("FILE", "STATUS")

# Download the gzip-compressed data files for the years specified
# Provide information on availability of file retrieval to working directory
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
    data$LAT <- data$LAT/1000
    data$LONG <- data$LONG/1000
    data$WIND.SPD <- data$WIND.SPD/10
    data$TEMP <- round((data$TEMP/10) + 273.2, 1)
    data$DEW.POINT <- data$DEW.POINT/10
    data$ATM.PRES <- data$ATM.PRES/10
    data$CEIL.HGT <- ifelse(data$CEIL.HGT == 99999, 9999, round(data$CEIL.HGT*3.28084/100, 0))
    
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
      precip_code <- rep(9999, length(additional.data$string))
      additional.data$PRECIP.CODE <- precip_code
    } 
    
    if (number_of_precip_lines == 0) {
      # put in vector of 9999 in PRECIP.CODE column of data frame
      additional.data$PRECIP.CODE <- rep(9999, length(additional.data$string))
    }
      
#     # opaque cloud cover: GA[1-6]
#     number_of_sky_cover_layer_coverage_code <- sum(str_detect(additional.data$string, "GA[1-6]"),
#                                                    na.rm = TRUE)
#     percentage_of_sky_cover_layer_coverage_code <- (number_of_sky_cover_layer_coverage_code/
#                                                       length(additional.data$string)) * 100
#     if (number_of_sky_cover_layer_coverage_code > 0) {
#       GA1_opaque_cloud_cover_in_tenths <- unlist(str_extract_all(additional.data$string,
#                                                                  "GA1[0-9][0-9]"))
#       GA1_opaque_cloud_cover_in_tenths <- str_replace_all(GA1_opaque_cloud_cover_in_tenths,
#                                                           "GA1([0-9][0-9])", "\\1")
#       GA1_opaque_cloud_cover_in_tenths <- as.numeric(GA1_opaque_cloud_cover_in_tenths)
#         if(GA1_opaque_cloud_cover_in_tenths == "00") {
#           0
#         } else if (GA1_opaque_cloud_cover_in_tenths == "01") {
#           1
#         } else if (GA1_opaque_cloud_cover_in_tenths == "02") {
#           2
#         } else if (GA1_opaque_cloud_cover_in_tenths == "03") {
#           4
#         } else if (GA1_opaque_cloud_cover_in_tenths == "04") {
#           5
#         } else if (GA1_opaque_cloud_cover_in_tenths == "05") {
#           6
#         } else if (GA1_opaque_cloud_cover_in_tenths == "06") {
#           7
#         } else if (GA1_opaque_cloud_cover_in_tenths == "07") {
#           9
#         } else if (GA1_opaque_cloud_cover_in_tenths == "08") {
#           10
#         } else if (GA1_opaque_cloud_cover_in_tenths == "09") {
#           10
#         } else if (GA1_opaque_cloud_cover_in_tenths == "10") {
#           10
#         } else if (GA1_opaque_cloud_cover_in_tenths == NULL) {
#           9999
#         } else {
#           9999
#         })
#       additional.data$OPAQUE.CC <- GA1_opaque_cloud_cover_in_tenths
#     }
#     
#     if (number_of_sky_cover_layer_coverage_code == 0) {
#       additional.data$OPAQUE.CC <- rep(9999, length(additional.data$string))
#     }
    
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
      additional.data$RH <- rep(9999, length(additional.data$string))
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


# Experimental: get additional metadata from stations
#  subset(read.csv("ish-history.csv"), USAF %in% stations$USAFID & WBAN %in% stations$WBAN)

# Write the station data to a CSV file
write.csv(stations, file = "stations.csv", row.names = FALSE)

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
               geom_rect(xmin = latlong_bbox_west, xmax = latlong_bbox_east,
                         ymin = latlong_bbox_south, ymax =latlong_bbox_north) +
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
               xlim(latlong_bbox_west, latlong_bbox_east) +
               ylim(latlong_bbox_south, latlong_bbox_north) +
               theme(legend.position = "none") +
               labs(x = "Longitude") +
               labs(y = "Latitude") +
               labs(title = "Plot of Surface Stations in Meteorological Domain")

# Read data from stations
# Remove any 999 values and make as NA, remove values where minutes are equal to 15, 30, or 45
st <- read.csv(file = paste(files[2], ".csv", sep = ""))
head(st)
percent_invalid_wind.dir <- ( sum(st$WIND.DIR == 9999.9 | st$WIND.DIR == 999.9) /
                                nrow(st) ) * 100
percent_invalid_atmos_pres <- ( sum(st$ATM.PRES == 9999.9 | st$ATM.PRES == 999.9) /
                                  nrow(st) ) * 100
st$WIND.DIR <- st$DEW.POINT <- st$ATM.PRES <- NULL
st$TEMP[st$TEMP == 999.9] <- NA
st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
st <- st[st$MIN == 0, ]

# For a SURF.DAT file, need to have the following parameters:
# (1)


# Variable  Description
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

# So far have the time elements (possibly still at +00h00), WD, WS, T (in degrees C?), and P





