define.calmet.domain <-
function(lat.dec.deg = NULL,
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

latlong_bbox <- mat.or.vec(4, 1)

latlong_bbox[1] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,1]
latlong_bbox[2] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1,2]
latlong_bbox[3] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,2]
latlong_bbox[4] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2,1]

return(latlong_bbox)
}
