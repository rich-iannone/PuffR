#' Define the CALMET domain and generate a geophysical input file
#' @description Define the CALMET domain and determine the best gridded values for land use, terrain heights, and micrometeorological parameters for creation of a geophysical input file.
#' @param location_name an assigned name for the CALMET domain.
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_lon_grid_loc argument.
#' @param lon_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_lon_grid_loc argument.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the desired width of the meteorological domain in meters.
#' @param domain_height_m the desired height of the meteorological domain in meters.
#' @param download_SRTM a choice of whether to download the SRTM GeoTIFF height data from a server or read the identical files from a local folder.
#' @param SRTM_file_path path to a folder containing a collection of SRTM V4 zip archive files.
#' @export calmet_define_geophys
#' @examples
#' \dontrun{
#' # Create a CALMET domain of 100 by 100 km in the Los Angeles area.
#' # Chosen lat/lon coordinates are for the center of the domain. 
#' calmet_define_geophys(location_name = "the_city",
#'                       lat_dec_deg = 34.050184,
#'                       lon_dec_deg = -118.253959,
#'                       lat_lon_grid_loc = 1,
#'                       domain_width_m = 8000,
#'                       domain_height_m = 8000,
#'                       download_SRTM = TRUE)
#'}

calmet_define_geophys <- function(location_name,
                                  lat_dec_deg = NULL,
                                  lon_dec_deg = NULL,
                                  lat_lon_grid_loc = 1,
                                  domain_width_m = NULL,
                                  domain_height_m = NULL,
                                  four_seasons = TRUE,
                                  four_season_breaks = c("03-15", "05-31", "08-31", "11-15"),
                                  download_SRTM = TRUE,
                                  SRTM_file_path = NULL){
  
  # Add require statements
  require(rgdal)
  require(plyr)
  require(sp)
  require(raster)
  require(ggplot2)
  require(stringr)
  require(MODISTools)
  
  # Convert the 'location_name' string to lowercase
  location_name <- tolower(location_name)
  
  # Define the cell resolution (square cells) as 250 m
  cell_resolution_m <- 250
  
  # Round the provided width and the height of the met domain to the resolution of the cell
  domain_width_m <- round_any(domain_width_m, cell_resolution_m, round)
  domain_height_m <- round_any(domain_height_m, cell_resolution_m, round)
  
  # Get matrix of longitude and latitude for chosen point
  lat_lon_dec_deg <- cbind(lon_dec_deg, lat_dec_deg)
  
  # Determine the UTM zone
  UTM_zone <- (floor((lon_dec_deg + 180)/6) %% 60) + 1
  
  # Determine whether domain is in Northern Hemisphere or Southern Hemisphere
  UTM_hemisphere <- ifelse(lat_dec_deg >= 0, "N", "S")
  
  # Define a PROJ.4 projection string for a lat/lon projection
  proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Define a PROJ.4 projection string for a UTM projection
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Project as UTM coordinates from the determined UTM zone, round to nearest 250 m using the
  # 'round_any' function from the 'plyr' package
  UTM_location <- project(lat_lon_dec_deg, proj_string_UTM)
  UTM_location <- round_any(UTM_location, cell_resolution_m, round)
  
  # Do these length and width values accomodate an integer number of cells of the specified resolution?
  # These checks will be later part of a function in setting domain width and height
  is_number_cells_across_x_an_int <- ifelse(domain_width_m %% cell_resolution_m != 0, FALSE, TRUE)
  is_number_cells_across_y_an_int <- ifelse(domain_height_m %% cell_resolution_m != 0, FALSE, TRUE)
  
  # Get the number of cells in the x direction
  number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                  domain_width_m/cell_resolution_m, NULL)
  
  # Get the number of cells in the y direction
  number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                  domain_height_m/cell_resolution_m, NULL)
  
  # Get the total number of cells
  total_cells <- number_cells_across_x * number_cells_across_y
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  left_UTM <- get_grid_extents_UTM(side = "left",
                                   lat_lon_grid_loc = lat_lon_grid_loc,
                                   UTM_location = UTM_location,
                                   domain_width_m = domain_width_m,
                                   domain_height_m = domain_height_m)
  
  right_UTM <- get_grid_extents_UTM(side = "right",
                                    lat_lon_grid_loc = lat_lon_grid_loc,
                                    UTM_location = UTM_location,
                                    domain_width_m = domain_width_m,
                                    domain_height_m = domain_height_m)
  
  bottom_UTM <- get_grid_extents_UTM(side = "bottom",
                                     lat_lon_grid_loc = lat_lon_grid_loc,
                                     UTM_location = UTM_location,
                                     domain_width_m = domain_width_m,
                                     domain_height_m = domain_height_m)
  
  top_UTM <- get_grid_extents_UTM(side = "top",
                                  lat_lon_grid_loc = lat_lon_grid_loc,
                                  UTM_location = UTM_location,
                                  domain_width_m = domain_width_m,
                                  domain_height_m = domain_height_m)
  
  # Create a data frame object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_DF <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                     "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
  
  # Create a SpatialPoints object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_SP <- SpatialPoints(as.matrix(LL_LR_UL_UR_UTM_m_DF),
                                        proj4string = CRS(proj_string_UTM))
  
  # Generate Extent object in UTM
  bbox_UTM <- extent(LL_LR_UL_UR_UTM_m_SP)
  
  # Create a RasterLayer object for UTM values
  LL_LR_UL_UR_UTM_m_RL <- raster(nrows = number_cells_across_x,
                                 ncols = number_cells_across_x,
                                 ext = bbox_UTM,
                                 crs = proj_string_UTM)
  
  # Create a SpatialPoints object for lat/lon values of LL, LR, UL, and UR through a
  # spatial transform
  LL_LR_UL_UR_longlat_SP <- spTransform(LL_LR_UL_UR_UTM_m_SP, CRS("+proj=longlat +ellps=GRS80"))
  
  # Obtain DEM data projected as long/lat for the domain as a RasterLayer object
  srtm <- download_SRTMV4_GeoTIFF(lon = floor(lon_dec_deg),
                                  lat = floor(lat_dec_deg),
                                  download = download_SRTM,
                                  SRTM_file_path = SRTM_file_path)
  
  # Generate Extents object in long/lat projection for cropping
  bbox_longlat <- extent(LL_LR_UL_UR_longlat_SP)
  
  # Crop DEM data using 'bbox' Extent object in lat/lon projection
  srtm_cropped <- crop(srtm, bbox_longlat)
  
  # Reproject cropped RasterLayer object from lat/lon to UTM
  srtm_UTM <- projectRaster(srtm_cropped,
                            crs = paste("+proj=utm +zone=",
                                        UTM_zone,
                                        " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                        sep = ''))
  
  # Crop DEM data again using 'bbox' Extent object in UTM projection
  srtm_UTM_resampled <- resample(srtm_UTM, LL_LR_UL_UR_UTM_m_RL)
  
  # Create a SpatialPixelsDataFrame from the resampled data
  srtm_UTM_resampled.SPDF <- as(srtm_UTM_resampled, "SpatialPixelsDataFrame")
  
  # Create a copy of the RasterLayer object for subsituting NA values with 0
  srtm_UTM_resampled_no_NA <- srtm_UTM_resampled
  
  # Substitute NA values with 0 values in RasterLayer copy
  srtm_UTM_resampled_no_NA@data@values[is.na(srtm_UTM_resampled_no_NA@data@values)] <- 0
  
  # Create a SpatialPixelsDataFrame from the resampled data with no NA values in the data/values slot
  srtm_UTM_resampled_no_NA.SPDF <- as(srtm_UTM_resampled_no_NA, "SpatialPixelsDataFrame")
  
  # Create a data frame for plotting in ggplot
  srtm_UTM_resampled.df <- as.data.frame(srtm_UTM_resampled.SPDF)
  
  # Change the column names to a standard set of labels
  colnames(srtm_UTM_resampled.df) <- c("z", "x", "y")
  
  # Plot the grid of heights using ggplot
  g <- ggplot(srtm_UTM_resampled.df, aes(x = x/1000, y = y/1000, fill = z)) +
    geom_tile(aes(fill = z)) +
    scale_fill_gradient(low = "green", high = "red",
                        guide = guide_legend(title = "Heights")) +
    coord_equal() +
    theme_bw(base_size = 12, base_family = "") +
    labs(x = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Easting, km", sep = '')) +
    labs(y = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Northing, km", sep = '')) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)))
  
  # Save terrain plot as a pdf file
  ggsave(filename = "terrain.pdf",
         width = 8, height = 8, units = "in")
  
  # Extract heights from the resampled DEM in UTM
  gridded_heights_UTM_m_vector <- srtm_UTM_resampled@data@values
  
  # Create a data frame for the extracted heights in UTM, in row-major order
  gridded_heights_UTM_m_df <- as.data.frame(t(matrix(gridded_heights_UTM_m_vector,
                                                     ncol = number_cells_across_y)))
  
  # Replace NA values with 0 values
  gridded_heights_UTM_m_df[is.na(gridded_heights_UTM_m_df)] <- 0
  
  # Create file header for GEO.DAT file
  geo_dat_h <- vector(mode = "character", length = 9)
  geo_dat_h[1] <- "GEO.DAT         2.0             Header structure with coordinate parameters"
  geo_dat_h[2] <- "2"
  geo_dat_h[3] <- "Produced by PuffR !Do not edit by hand!"
  geo_dat_h[4] <- "Time Information"
  geo_dat_h[5] <- "UTM"
  geo_dat_h[6] <- paste("  ", UTM_zone, UTM_hemisphere, sep = '')
  geo_dat_h[7] <- "WGS-84  02-21-2003"
  geo_dat_h[8] <- paste("     ", number_cells_across_x,
                        "     ", number_cells_across_y,
                        "     ", round(left_UTM/1000, digits = 3),
                        "     ", round(bottom_UTM/1000, digits = 3),
                        "     ", round(cell_resolution_m/1000, digits = 3),
                        "     ", round(cell_resolution_m/1000, digits = 3),
                        sep = '')
  geo_dat_h[9] <- "KM  M"   
  
  # Generate a vector of comma-delimited strings containing heights of every row of cells;
  # this is for writing to a file and eventual inclusion in the GEO.DAT file
  for (i in 1:nrow(gridded_heights_UTM_m_df)){
    if (i == 1) gridded_heights_UTM_m_row_major_strings <- vector(mode = "character", length = 0)
    string <- paste(round(gridded_heights_UTM_m_df[i, ], digits = 2), collapse = ", ")
    gridded_heights_UTM_m_row_major_strings <- c(gridded_heights_UTM_m_row_major_strings, string)
  }
  
  # Write the heights category subheader and data to disk
  geo_dat_h_heights <- " 1.0000  - TERRAIN heights - HTFAC (Conversion to meters)"
  
  # Create data frame for MODIS IGBP Type 1 codes for land cover
  IGBP_Type_1_class_no <- c(seq(0, 16, 1), 254, 255)
  IGBP_Type_1_class_name <- c("Water", "Evergreen needleleaf forest", "Evergreen broadleaf forest",
                              "Deciduous needleleaf forest", "Deciduous broadleaf forest",
                              "Mixed forest", "Closed shrublands", "Open shrublands",
                              "Woody savannas", "Savannas", "Grasslands", "Permanent wetlands",
                              "Croplands", "Urban and built-up", "Cropland/Natural vegetation mosaic",
                              "Snow and ice", "Barren or sparsely vegetated", "Unclassified",
                              "Fill value")
  CALMET_categories <- c(50, 40, 40, 40, 40, 40, 40, 40, 30, 30,
                         30, 60, 20, 10, 20, 90, 70, NA, NA)
  
  LU_classification <- data.frame(IGBP_Type_1_class_no, IGBP_Type_1_class_name, CALMET_categories,
                                  stringsAsFactors = FALSE)
  
  # Create a RasterLayer object with lat/lon coordinates for grid cells
  srtm_latlon_RL <- raster(bbox_longlat,
                           nrows = number_cells_across_y,
                           ncols = number_cells_across_x,
                           crs = proj_string_longlat)
  
  # Create a SpatialPixels object from the generated RasterLayer object
  srtm_latlon_SP <- as(srtm_latlon_RL, "SpatialPixels")
  
  # Extract lat/lon coordinates from 'srtm_latlon_SP'
  modis_coordinates <- as.data.frame(srtm_latlon_SP@coords)
  colnames(modis_coordinates) <- c("long", "lat")
  
  # Create vectors of starting and ending dates for the land cover data
  start.date <- rep(2008, nrow(modis_coordinates))
  end.date <- rep(2008, nrow(modis_coordinates))
  
  # Column-bind the 'start.date' and 'end.date' vectors with the coordinates data frame
  modis_coordinates <- cbind(modis_coordinates, start.date)
  modis_coordinates <- cbind(modis_coordinates, end.date)
  
  # Acquire subsets of the landcover Type 1 codes from the MODIS MCD12Q1 product 
  MODISSubsets(LoadDat = modis_coordinates, Products = "MCD12Q1",
               Bands = c("Land_Cover_Type_1"),
               Size = c(0,0), TimeSeriesLength = 1)
  
  # Generate a file list of acquired MODIS data for each set of coordinates
  file_list <- list.files(pattern = ".*_MCD12Q1.asc")
  
  # Extract the land use code from each acquired data file
  for (i in 1:length(file_list)){
    if (i == 1) IGBP_Type_1_class_no <- vector(mode = "numeric", length = 0)
    class_no <- 
      as.numeric(unlist(str_split(readLines(con = file_list[i])[1],
                                  pattern = ","))[length(unlist(str_split(readLines(con = file_list[i])[1],
                                                                          pattern = ",")))])
    IGBP_Type_1_class_no <- c(IGBP_Type_1_class_no, class_no)
  }
  
  # Delete the .asc files from the working folder
  #   file.remove(file_list)
  
  # Delete the summary CSV file from the working folder
  #   file.remove(list.files(pattern = "Subset Download.*.csv"))
  
  # Get the corresponding CALMET category from the IGBP Type 1 class data
  CALMET_categories <- join(as.data.frame(IGBP_Type_1_class_no), LU_classification)[,3]
  
  # Create a data frame for the LU categories, in row-major order
  gridded_CALMET_categories <- as.data.frame(t(matrix(CALMET_categories,
                                                      ncol = number_cells_across_y)))
  
  # Generate a vector of comma-delimited strings containing LU categories of every row of cells;
  # this is for writing to a file and eventual inclusion in the GEO.DAT file
  for (i in 1:nrow(gridded_CALMET_categories)){
    if (i == 1) gridded_CALMET_categories_strings <- vector(mode = "character", length = 0)
    string <- paste(gridded_CALMET_categories[i, ], collapse = ", ")
    gridded_CALMET_categories_strings <- c(gridded_CALMET_categories_strings, string)
  }
  
  # Create new data frame object 'UTM_gridded_values' that contains gridded heights and
  # LU categories
  UTM_gridded_values <- cbind(srtm_UTM_resampled_no_NA.SPDF, as.data.frame(CALMET_categories))
  
  # Force values in the 'CALMET_categories' column of the 'UTM_gridded_values' data frame to
  # be 50 (water) if height is 0
  for (i in 1:nrow(UTM_gridded_values)){
    if (UTM_gridded_values[i,1] == 0.00000) UTM_gridded_values[i,4] <- 50
  }
  
  # Replace 'CALMET_categories' vector with revised values
  CALMET_categories <- UTM_gridded_values$CALMET_categories
  
  # Define the colours for each of the CALMET land use categories using a named vector
  cols <- c("10" = "gold2",
            "20" = "olivedrab2",
            "30" = "springgreen",
            "40" = "forestgreen",
            "50" = "deepskyblue2",
            "60" = "orchid",
            "70" = "lightsalmon",
            "80" = "moccasin",
            "90" = "honeydew")
  
  # Reclass 'CALMET_categories' as a factor for the purpose of generating a ggplot object
  UTM_gridded_values$CALMET_categories <- as.factor(UTM_gridded_values$CALMET_categories)
  
  # Plot the grid of land use categories using ggplot
  h <- ggplot(UTM_gridded_values, aes(x = x/1000, y = y/1000,
                                      fill = CALMET_categories)) +
    geom_tile() +
    scale_fill_manual(values = cols,
                      breaks = c(as.numeric(names(cols)), 100),
                      name = "Land Use\nCategories") +
    coord_equal() +
    theme_bw(base_size = 12, base_family = "") +
    labs(x = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Easting, km", sep = '')) +
    labs(y = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Northing, km", sep = '')) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)))
  
  # Save as land use plot as a pdf file
  ggsave(filename = "landuse.pdf", device = pdf,
         width = 8, height = 8, units = "in")
  
  # Get "CALMET_categories" as a numeric object
  UTM_gridded_values$CALMET_categories <-
    as.numeric(as.character(UTM_gridded_values$CALMET_categories))
    
  # Write the LU category subheader and data to disk
  geo_dat_h_LU <- "0  --- LAND USE CATEGORIES  0 - DEFAULT CATEGORIES  1 - NEW CATEGORIES"
  
  # Create vector of short descriptions for each micrometeorological parameter
  mmet_descriptions <- c("gridded z0 field",
                         "gridded albedo field",
                         "gridded Bowen ratio field",
                         "gridded soil heat flux parameters",
                         "gridded anthropogenic heat flux field",
                         "gridded leaf area index field")
  
  if (four_seasons == TRUE){
    
    # Get data frame containing micrometeorological parameters by land use category by season
    mmet_seasons <- calmet_seasonal_micrometeorology()
    
    # Get the corresponding micrometeorological parameters by gridded CALMET category by season
    mmet_winter <- join(data.frame(CALMET_categories = CALMET_categories),
                        subset(mmet_seasons, season == "Winter"))
    
    mmet_spring <- join(data.frame(CALMET_categories = CALMET_categories),
                        subset(mmet_seasons, season == "Spring"))
    
    mmet_summer <- join(data.frame(CALMET_categories = CALMET_categories),
                        subset(mmet_seasons, season == "Summer"))
    
    mmet_fall <- join(data.frame(CALMET_categories = CALMET_categories),
                      subset(mmet_seasons, season == "Fall"))
    
    # Generate the 5 GEO.DAT filenames
    geo_dat_filenames <- c(paste("geo--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--1-winter.txt",
                                 sep = ''),
                           paste("geo--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--2-spring.txt",
                                 sep = ''),
                           paste("geo--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--3-summer.txt",
                                 sep = ''),
                           paste("geo--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--4-fall.txt",
                                 sep = ''),
                           paste("geo--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--5-winter.txt",
                                 sep = ''))
    
    # Create Winter GEO.DAT files
    for (i in 2:7){
      if (i == 2){
        cat(file = geo_dat_filenames[1],
            append = FALSE)
        geo_dat_h[4] <- paste("1 Winter (01 01 - ",
                              gsub("-", " ", four_season_breaks[1]),
                              ")", sep = '')
        cat(geo_dat_h, file = geo_dat_filenames[1],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_LU, file = geo_dat_filenames[1],
            sep = "\n", append = TRUE)
        cat(gridded_CALMET_categories_strings, file = geo_dat_filenames[1],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_heights, file = geo_dat_filenames[1],
            sep = "\n", append = TRUE)
        cat(gridded_heights_UTM_m_row_major_strings, file = geo_dat_filenames[1],
            sep = "\n", append = TRUE)
      }
      cat(paste(" 2    - ", mmet_descriptions[i - 1], sep = ''),
          file = geo_dat_filenames[1],
          sep = "\n", append = TRUE)
      cat(vector_values_to_row_major_strings(values_vector = mmet_winter[,i],
                                             number_cells_across_y = number_cells_across_y),
          file = geo_dat_filenames[1],
          sep = "\n", append = TRUE)
    }
    
    for (i in 2:7){
      if (i == 2){
        cat(file = geo_dat_filenames[5],
            append = FALSE)
        geo_dat_h[4] <- paste("5 Winter (",
                              format((as.Date(four_season_breaks[4], "%m-%d") + 1), "%m %d"),
                              " - 12 31)", sep = '')
        cat(geo_dat_h, file = geo_dat_filenames[5],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_LU, file = geo_dat_filenames[5],
            sep = "\n", append = TRUE)
        cat(gridded_CALMET_categories_strings, file = geo_dat_filenames[5],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_heights, file = geo_dat_filenames[5],
            sep = "\n", append = TRUE)
        cat(gridded_heights_UTM_m_row_major_strings, file = geo_dat_filenames[5],
            sep = "\n", append = TRUE)
      }
      cat(paste(" 2    - ", mmet_descriptions[i - 1], sep = ''),
          file = geo_dat_filenames[5],
          sep = "\n", append = TRUE)
      cat(vector_values_to_row_major_strings(values_vector = mmet_winter[,i],
                                             number_cells_across_y = number_cells_across_y),
          file = geo_dat_filenames[5],
          sep = "\n", append = TRUE)
    }
    
    
    # Create Spring GEO.DAT file
    for (i in 2:7){
      if (i == 2){
        cat(file = geo_dat_filenames[2],
            append = FALSE)
        geo_dat_h[4] <- paste("2 Spring (",
                              format((as.Date(four_season_breaks[1], "%m-%d") + 1), "%m %d"),
                              " - ",
                              gsub("-", " ", four_season_breaks[2]), ")", sep = '')
        cat(geo_dat_h, file = geo_dat_filenames[2],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_LU, file = geo_dat_filenames[2],
            sep = "\n", append = TRUE)
        cat(gridded_CALMET_categories_strings, file = geo_dat_filenames[2],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_heights, file = geo_dat_filenames[2],
            sep = "\n", append = TRUE)
        cat(gridded_heights_UTM_m_row_major_strings, file = geo_dat_filenames[2],
            sep = "\n", append = TRUE)
      }
      cat(paste(" 2    - ", mmet_descriptions[i - 1], sep = ''),
          file = geo_dat_filenames[2],
          sep = "\n", append = TRUE)
      cat(vector_values_to_row_major_strings(values_vector = mmet_spring[,i],
                                             number_cells_across_y = number_cells_across_y),
          file = geo_dat_filenames[2],
          sep = "\n", append = TRUE)
    }
    
    # Create Summer GEO.DAT file
    for (i in 2:7){
      if (i == 2){
        cat(file = geo_dat_filenames[3],
            append = FALSE)
        geo_dat_h[4] <- paste("3 Summer (",
                              format((as.Date(four_season_breaks[2], "%m-%d") + 1), "%m %d"),
                              " - ",
                              gsub("-", " ", four_season_breaks[3]), ")", sep = '')
        cat(geo_dat_h, file = geo_dat_filenames[3],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_LU, file = geo_dat_filenames[3],
            sep = "\n", append = TRUE)
        cat(gridded_CALMET_categories_strings, file = geo_dat_filenames[3],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_heights, file = geo_dat_filenames[3],
            sep = "\n", append = TRUE)
        cat(gridded_heights_UTM_m_row_major_strings, file = geo_dat_filenames[3],
            sep = "\n", append = TRUE)
      }
      cat(paste(" 2    - ", mmet_descriptions[i - 1], sep = ''),
          file = geo_dat_filenames[3],
          sep = "\n", append = TRUE)
      cat(vector_values_to_row_major_strings(values_vector = mmet_summer[,i],
                                             number_cells_across_y = number_cells_across_y),
          file = geo_dat_filenames[3],
          sep = "\n", append = TRUE)
    }
    
    # Create Fall GEO.DAT file
    for (i in 2:7){
      if (i == 2){
        cat(file = geo_dat_filenames[4], append = FALSE)
        geo_dat_h[4] <- paste("4 Fall (",
                              format((as.Date(four_season_breaks[3], "%m-%d") + 1), "%m %d"),
                              " - ",
                              gsub("-", " ", four_season_breaks[4]), ")", sep = '')
        cat(geo_dat_h, file = geo_dat_filenames[4],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_LU, file = geo_dat_filenames[4],
            sep = "\n", append = TRUE)
        cat(gridded_CALMET_categories_strings, file = geo_dat_filenames[4],
            sep = "\n", append = TRUE)
        cat(geo_dat_h_heights, file = geo_dat_filenames[4],
            sep = "\n", append = TRUE)
        cat(gridded_heights_UTM_m_row_major_strings, file = geo_dat_filenames[4],
            sep = "\n", append = TRUE)
      }
      cat(paste(" 2    - ", mmet_descriptions[i - 1], sep = ''),
          file = geo_dat_filenames[4],
          sep = "\n", append = TRUE)
      cat(vector_values_to_row_major_strings(values_vector = mmet_fall[,i],
                                             number_cells_across_y = number_cells_across_y),
          file = geo_dat_filenames[4],
          sep = "\n", append = TRUE)
    }
    
  }
  
}
