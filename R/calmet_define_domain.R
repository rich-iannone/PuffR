#' Define the CALMET domain and determine gridded terrain heights
#' @description Define the CALMET domain and determine gridded terrain heights for use in the GEO.DAT CALMET input file
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param long_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lat_long_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the desired width of the meteorological domain in meters.
#' @param domain_height_m the desired height of the meteorological domain in meters.
#' @export calmet_define_domain
#' @examples
#' \dontrun{
#' # Create a CALMET domain of 100 by 100 km in the Los Angeles area.
#' # Chosen lat/long coordinates are for the center of the domain. 
#' calmet_define_domain(lat_dec_deg = 34.050184,
#'                      long_dec_deg = -118.253959,
#'                      lat_long_grid_loc = 1,
#'                      EPSG_code = 32611,
#'                      projection = "UTM",
#'                      datum = "WGS84",
#'                      ellipse = "WGS84",
#'                      units = "m",
#'                      cell_resolution_m = 250,
#'                      domain_width_m = 100000,
#'                      domain_height_m = 100000)
#'}

calmet_define_domain <- function(lat_dec_deg = NULL,
                                 long_dec_deg = NULL,
                                 lat_long_grid_loc = 1,
                                 EPSG_code = NULL,
                                 projection = "UTM",
                                 datum = "WGS84",
                                 ellipse = "WGS84",
                                 units = "m",
                                 cell_resolution_m = 250,
                                 domain_width_m = NULL,
                                 domain_height_m = NULL) {

require(rgdal)
require(plyr)
  
#lat_dec_deg <- 34.050184
#long_dec_deg <- -118.253959

#EPSG_code <- 32611

# projection <- "UTM"
# datum <- "WGS84"
# ellipse <- "WGS84"
# units <- "m"

# Where is this point located on the grid?
# Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
#lat_long_grid_loc <- 1
  
# what is the the cell resolution (square cells) required (in meters)?
#cell_resolution_m <- 250

# What is the width and the height of the met domain in meters? Extents will be generated based on the
# location of chosen point.
#domain_width_m <- 100000
#domain_height_m <- 100000

lat_long_dec_deg <- cbind(long_dec_deg, lat_dec_deg)
UTM_zone <- (floor((long_dec_deg + 180)/6) %% 60) + 1

# Generate a PROJ.4 string from an acceptable EPSG code
# Include a dataframe that contains all acceptable EPSG codes for filtering
# Information on this page: http://cicero.azavea.com/docs/epsg_codes.html
data(EPSG_proj4)
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
is_number_cells_across_x_an_int <- ifelse(domain_width_m %% cell_resolution_m != 0, FALSE, TRUE)
is_number_cells_across_y_an_int <- ifelse(domain_height_m %% cell_resolution_m != 0, FALSE, TRUE)

number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                domain_width_m/cell_resolution_m, NULL)

number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                domain_height_m/cell_resolution_m, NULL)

total_cells <- number_cells_across_x * number_cells_across_y

# Get extents of UTM grid (left, right, bottom, top)
left_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,1] - (0.5 * domain_width_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,1]
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,1] - domain_width_m
          } else if (lat_long_grid_loc == 4) {
            UTM_location[1,1]
          } else if (lat_long_grid_loc == 5) {
            UTM_location[1,1] - domain_width_m
          } else {
          NULL
          }

right_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,1] + (0.5 * domain_width_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,1] + domain_width_m
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,1]
          } else if (lat_long_grid_loc == 4) {
          UTM_location[1,1] + domain_width_m
          } else if (lat_long_grid_loc == 5) {
          UTM_location[1,1]
          } else {
          NULL
          }

bottom_UTM <- if(lat_long_grid_loc == 1) {
          UTM_location[1,2] - (0.5 * domain_height_m)
          } else if (lat_long_grid_loc == 2) {
          UTM_location[1,2]
          } else if (lat_long_grid_loc == 3) {
          UTM_location[1,2]
          } else if (lat_long_grid_loc == 4) {
          UTM_location[1,2] - domain_height_m
          } else if (lat_long_grid_loc == 5) {
          UTM_location[1,2] - domain_height_m
          } else {
          NULL
          }

top_UTM <- if(lat_long_grid_loc == 1) {
        UTM_location[1,2] + (0.5 * domain_height_m)
        } else if (lat_long_grid_loc == 2) {
        UTM_location[1,2] + domain_height_m
        } else if (lat_long_grid_loc == 3) {
        UTM_location[1,2] + domain_height_m
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
                               proj4string = CRS(paste("+init=epsg:", EPSG_code, sep = "")))

LL_LR_UL_UR_UTM_longlat <- spTransform(LL_LR_UL_UR_UTM_m, CRS("+proj=longlat +ellps=GRS80"))

LL_LR_UL_UR_UTM_longlat.df <- as.data.frame(LL_LR_UL_UR_UTM_longlat)

calmet_define_domain_out <- mat.or.vec(7, 1)

# Return vector with: [1] - [4] bounding box lat/long coordinates (W, E, N, S),
# [5] number of cells in x direction, [6] number of cells in y direction,
# [7] total number of cells
calmet_define_domain_out[1] <- LL_LR_UL_UR_UTM_longlat.df[1,1]
calmet_define_domain_out[2] <- LL_LR_UL_UR_UTM_longlat.df[1,2]
calmet_define_domain_out[3] <- LL_LR_UL_UR_UTM_longlat.df[2,2]
calmet_define_domain_out[4] <- LL_LR_UL_UR_UTM_longlat.df[2,1]
calmet_define_domain_out[5] <- number_cells_across_x
calmet_define_domain_out[6] <- number_cells_across_y
calmet_define_domain_out[7] <- total_cells

write.table(calmet_define_domain_out, file = "calmet_define_domain.out",
            col.names = FALSE, row.names = FALSE)

print(define.calmet.domain.out)

}
