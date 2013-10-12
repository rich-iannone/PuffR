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

## Need to obtain spatial points in lat/lon for LL, LR, UL, UR

LL_LR_UL_UR_m <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))

LL_LR_UL_UR_UTM_m <- SpatialPoints(as.matrix(LL_LR_UL_UR_m),
                               proj4string=CRS("+init=epsg:32610"))

LL_LR_UL_UR_UTM_longlat <- spTransform(LL_LR_UL_UR_UTM_m, CRS("+proj=longlat +ellps=GRS80"))


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
    outputs[s, 2] <- ifelse(file.exists(outputs[s, 1]) == "TRUE", 'available', 'missing')
  }
}

# Extract all downloaded data files
system("gunzip *.gz", intern = FALSE, ignore.stderr = TRUE)

# Attempt to read data from files

files <- list.files(pattern = "^[0-9]*-[0-9]*-[0-9]*$")

column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1,
                   5, 1, 1, 1, 6, 1, 1, 1, 5, 1,
                   5, 1, 5, 1)

stations <- as.data.frame(matrix(NA, length(files), 6))

names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")

for (i in 1:length(files)) {
    data <- read.fwf(files[i], column.widths)
    data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33)]
    names(data) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN", "LAT", "LONG",
                     "ELEV", "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT", "ATM.PRES")
    data$LAT <- data$LAT/1000
    data$LONG <- data$LONG/1000
    data$WIND.SPD <- data$WIND.SPD/10
    data$TEMP <- data$TEMP/10
    data$DEW.POINT <- data$DEW.POINT/10
    data$ATM.PRES <- data$ATM.PRES/10
    write.csv(data, file = paste(files[i], ".csv", sep = ""), row.names = FALSE)
    stations[i, 1:3] <- data[1, 1:3]
    stations[i, 4:6] <- data[1, 8:10]
}

write.csv(stations, file = "stations.csv", row.names = FALSE)

canada_raster <- raster("CAN_alt.gri")

# Ctop raster to domain + a margin of 0.2 degrees


LL_LR_UL_UR_UTM_longlat

print(coordinates(LL_LR_UL_UR_UTM_longlat), digits=12)[1,1]

extent_in_lat_long <- extent(coordinates(LL_LR_UL_UR_UTM_longlat)[1,1],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[2,1],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[1,2],
                             coordinates(LL_LR_UL_UR_UTM_longlat)[3,2])


domain_raster <- crop(canada_raster, extent_in_lat_long)

plot(domain_raster)
points(stations$LONG, stations$LAT, pch = 16, col = "black")
points(lat_long_dec_deg[1,1], lat_long_dec_deg[1,2], pch = 15, col = "red")





# Read data from stations
# Remove any values where minutes are equal to 15, 30, or 45


st <- read.csv(file = paste(files[1], ".csv", sep = ""))
head(st)

percent_invalid_wind.dir <- ( sum(st$WIND.DIR == 9999.9 | st$WIND.DIR == 999.9) / nrow(st) ) * 100

percent_invalid_atmos_pres <- ( sum(st$ATM.PRES == 9999.9 | st$ATM.PRES == 999.9) / nrow(st) ) * 100


st$WIND.DIR <- st$DEW.POINT <- st$ATM.PRES <- NULL
st$TEMP[st$TEMP == 999.9] <- NA
st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
st <- st[st$MIN == 0, ]

