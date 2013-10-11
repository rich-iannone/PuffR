##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
##                                                                  ##
## R project to generate useful tool for dispersion modelling with  ##
## the CALPUFF system                                               ##
##                                                                  ##
## Rich Iannone, 2013                                               ##
##                                                                  ##
##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##

# Load in necessary packages
install.packages("sp")
library("sp")
install.packages("rgdal")
library("rgdal")
install.packages("plyr")
library("plyr")

# Start with information on location for CALMET domain
# Center of domain? LL? UR?
# Coordinate Systems?

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

# Get extents of grid

# Westing is [1,1], Northing is [1,2]
UTM_location[1,1]

# Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)


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

# Get surface data from NCDC, download via FTP

# 



# Determine which surface stations are in the domain



# Construct FTP address