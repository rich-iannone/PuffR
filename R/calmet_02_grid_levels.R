#' Set CALMET parameters for the map projection, datum, and grid definitions
#' @description This function validates and writes CALMET parameters for the map projection, datum, and grid definitions to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param pmap the projection of the CALMET domain.
#' @param feast the false easting at the projection origin for TTM, LCC, or LAZA projection types.
#' @param fnorth the false northing at the projection origin for TTM, LCC, or LAZA projection types.
#' @param iutmzn the UTM zone.
#' @param utmhem the UTM hemisphere.
#' @param rlat0 latitude (decimal degrees) of projection origin for TTM, LCC, PS, EM, or LAZA projection types.
#' @param rlon0 longitude (decimal degrees) of projection origin for TTM, LCC, PS, EM, or LAZA projection types.
#' @param xlat1 the lower matching parallel of latitude (decimal degrees) for LCC or PS projection types.
#' @param xlat2 the upper matching parallel of latitude (decimal degrees) for LCC or PS projection types.
#' @param datum the datum-region for output coordinates.
#' @param nx the number of grid cells in the x direction.
#' @param ny the number of grid cells in the y direction.
#' @param dgridkm the grid spacing in units of km.
#' @param xorigkm the reference grid x coordinate (in km) of the southwest corner of grid cell (1, 1).
#' @param yorigkm the reference grid y coordinate (in km) of the southwest corner of grid cell (1, 1).
#' @param nz the number of vertical levels.
#' @param zface a vector containing cell face heights in meters.
#' @export calmet_02_grid_levels

calmet_02_grid_levels <- function(calmet_inp,
                                  read_xy_from_geo_dat = TRUE,
                                  pmap = "UTM",
                                  feast = 0.0,
                                  fnorth = 0.0,
                                  iutmzn = NULL,
                                  utmhem = NULL,
                                  rlat0 = "40N",
                                  rlon0 = "90W",
                                  xlat1 = "30N",
                                  xlat2 = "60N",
                                  datum = "WGS-84",
                                  nx = NULL,
                                  ny = NULL,
                                  dgridkm = NULL,
                                  xorigkm = NULL,
                                  yorigkm = NULL,
                                  nz = 12,
                                  zface = c(0,20,40,80,100,150,200,300,400,800,1400,2000,3000)){
  
  # Define the valid grid projections
  possible_projections <- c("UTM", "TTM", "LCC", "PS", "EM", "LAZA")
  
  # Change NULL values for certain arguments to NA values
  if (is.null(iutmzn)) iutmzn <- NA
  if (is.null(utmhem)) utmhem <- NA
  if (is.null(nx)) nx <- NA
  if (is.null(ny)) ny <- NA
  if (is.null(dgridkm)) dgridkm <- NA
  if (is.null(xorigkm)) xorigkm <- NA
  if (is.null(yorigkm)) yorigkm <- NA
  
  # Stop function of pmap not part of 'possible_projections' set
  if (!(pmap %in% possible_projections)){
    stop("The chosen projection is not valid.")
  }
    
  # Verify that 'nx' and 'ny' are non-zero, positive, integer values
  if (nx == 0 | nx == 0){
    stop("nx or ny cannot be equal to 0.")
  }
  
  if (nx < 0 | ny < 0){
    stop("nx or ny cannot be negative numbers.")
  }
  
  if (nx %% 1 != 0 | ny %% 1 != 0){
    stop("nx or ny must be integer values.")
  }
  
  # Verify that 'nz' has a starting '0' level and that there are 'nz + 1'
  # total levels with no duplication in levels
  if (!(0 %in% zface)){
    stop("A '0' level must be included in the 'zface' parameter list")
  }
  
  if (length(zface) != (nz + 1)){
    stop("The total number of levels specified must be of length 'nz' + 1")
  }
  
  if (any(duplicated(zface))){
    stop("The levels specified must all be unique")
  }
  
  # If option set to read x,y data from GEO.DAT file, get the relevant values
  if (read_xy_from_geo_dat == TRUE){
    
    # Generate vector list of all GEO.DAT files in the working folder
    geo_dat_file <- list.files(pattern = "geo--.*")
    
    # If there are multiple GEO.DAT files in the working folder, choose only
    # the first of the set
    if (length(geo_dat_file > 1)) geo_dat_file <- geo_dat_file[1]
    
    # Obtain several lines from the header portion of the GEO.DAT file
    geo_dat_header <- readLines(geo_dat_file)[
      (as.numeric(readLines(geo_dat_file)[2]) + 4):
        (as.numeric(readLines(geo_dat_file)[2]) + 6)]
    
  # Generate a formatted character string for 'zface'
  zface <- paste(zface, collapse = ", ")
  
  ####
  # Add parameters to working 'calmet.inp' file
  ####
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp)
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("PMAP", "FEAST", "FNORTH", "IUTMZN", "UTMHEM", "RLAT0", "RLON0", 
                "XLAT1", "XLAT2", "DATUM", "NX", "NY", "DGRIDKM", "XORIGKM", "YORIGKM", 
                "NZ")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(pmap, feast, fnorth, iutmzn, utmhem, rlat0, rlon0, 
                    xlat1, xlat2, datum, nx, ny, dgridkm, xorigkm, yorigkm, 
                    nz)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Add formatted 'zface' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "ZFACE",
                                       replacement = zface) 
  
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
