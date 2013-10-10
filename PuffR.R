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

# Start with information on location for CALMET domain
# Center of domain? LL? UR?
# Coordinate Systems?

# Let's start with lat/long in decimal degrees, convert to UTM, then develop a grid

lat_dec_deg <- 49.062145
long_dec_deg <- -123.128404
lat_long_dec_deg <- cbind(long_dec_deg, lat_dec_deg)

# Conversion to UTM should occur; first determine UTM zone
UTM_zone <- (floor((long_dec_deg + 180)/6) %% 60) + 1

# Generate a PROJ.4 string from an acceptable EPSG code
# Include a dataframe that contains all acceptable EPSG codes for filtering
# Information on this page: http://cicero.azavea.com/docs/epsg_codes.html

# Use the following code for testing purposes: 32610 (WGS 84 / UTM zone 10N)
# Should be the PROJ.4 string:
# +init=epsg:32610 +proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 

projection <- c("utm")
UTM_zone
datum <- c("WGS84")
units <- c("m")

# NOTE: with the above-entered info, should be able to obtain one EPSG code from a lookup data frame
EPSG_code <- 32610
proj_string <- c("+init=epsg:32610 +proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Project as UTM coordinate
UTM_location <- project(lat_long_dec_deg, proj_string)

# Where is this point located on the grid?
# Choices are: C (center), LL, LR, UL, UR
lat_long_grid_loc <- c("C")

# what is the the cell resolution (square cells) required (in meters)?
cell_resolution_m <- 50

# What is the width and the height of the met domain in meters? Extents will be generated based on the
# location of chosen point.
width_met_domain_m <- 30000
height_met_domain_m <- 30000

# Do these length and width values accomodate an integer number of cells of the specified resolution?
# These checks will be later part of a function in setting domain width and height
is_number_cells_across_x_an_int <- ifelse(width_met_domain_m %% cell_resolution_m != 0, FALSE, TRUE)
is_number_cells_across_y_an_int <- ifelse(width_met_domain_m %% cell_resolution_m != 0, FALSE, TRUE)

number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                width_met_domain_m/cell_resolution_m, NULL)

number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                height_met_domain_m/cell_resolution_m, NULL)


# Get surface data from NCDC, download via FTP


# Determine which surface stations are in the domain

# Construct FTP address