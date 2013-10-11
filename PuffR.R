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
install.packages("plyr")
library("plyr")
install.packages("spdep")
library("spdep")
install.packages("fields")
library("fields")
install.packages("MBA")
library("MBA")


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

# Project as UTM coordinate, round to nearest 50 m using round_any function ('plyr' package)
UTM_location <- project(lat_long_dec_deg, proj_string)
UTM_location <- round_any(UTM_location, 50, round)

# Where is this point located on the grid?
# Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
lat_long_grid_loc <- 1

# what is the the cell resolution (square cells) required (in meters)?
cell_resolution_m <- 50

# What is the width and the height of the met domain in meters? Extents will be generated based on the
# location of chosen point.
met_domain_width_m <- 30000
met_domain_height_m <- 30000

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


##----- SURF.DAT ---- Surface data

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

# Read in the CSV file
st <- read.csv("ish-history.csv")

# Get formatted list of stations from Canada ("CN")
names(st)[c(3, 10)] <- c("NAME", "ELEV")
st <- st[, -5]
st <- st[st$CTRY == "CN", ]

# Reintroduce the decimal
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10

# Recompose the years from the data file
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

# Generate a list of stations for BC based on beginning and end years, ignoring stations without
# beginning years reported.
bc.list <- st[st$STATE == "BC" & (st$BEGIN <= 2010 &
                                    st$END >= 2012 & !is.na(st$BEGIN)), ]

# Initialize data frame for file status reporting
outputs <- as.data.frame(matrix(NA, dim(bc.list)[1], 2))
names(outputs) <- c("FILE", "STATUS")

# Download the gzip-compressed data files for 2010 (in this case)
for (y in 2010:2010) {
  y.bc.list <- bc.list[bc.list$BEGIN <= y & bc.list$END >= y, ]
  for (s in 1:dim(y.bc.list)[1]) {
    outputs[s, 1] <- paste(sprintf("%06d", y.bc.list[s,1]),
                           "-", sprintf("%05d", y.bc.list[s,2]),
                           "-", y, ".gz", sep = "")
    system(paste("curl -O ftp://ftp3.ncdc.noaa.gov/pub/data/noaa/", y,
                     "/", outputs[s, 1], sep = ""))
  }
}

system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)
