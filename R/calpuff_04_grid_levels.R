#' Set CALPUFF parameters for the map projection, datum, and grid definitions
#' @description This function validates and writes CALPUFF parameters for the map projection, datum, and grid definitions to the working CALPUFF.INP file.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param pmap  the projection of the CALPUFF domain.
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
#' @param ibcomp the x index at the lower-left corner of the computational grid.
#' @param jbcomp the y index at the lower-left corner of the computational grid.
#' @param iecomp the x index at the upper-right corner of the computational grid.
#' @param jecomp the y index at the upper-right corner of the computational grid.
#' @param lsamp the choice of whether gridded receptors are to be used.
#' @param ibsamp the x index at the lower-left corner of the sampling grid.
#' @param jbsamp the y index at the lower-left corner of the sampling grid.
#' @param iesamp the x index at the upper-right corner of the sampling grid.
#' @param jesamp the y index at the upper-right corner of the sampling grid.
#' @param meshdn the nesting factor of the sampling grid.
#' @export calpuff_04_grid_levels

calpuff_04_grid_levels <- function(calpuff_inp = "calpuff_template.txt",
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
                                   zface = NULL,
                                   ibcomp = NULL,
                                   jbcomp = NULL,
                                   iecomp = NULL,
                                   jecomp = NULL,
                                   lsamp = NULL,
                                   ibsamp = NULL,
                                   jbsamp = NULL,
                                   iesamp = NULL,
                                   jesamp = NULL,
                                   meshdn = 1){
  
  # Change NULL values for certain arguments to NA values
  if (is.null(iutmzn)) iutmzn <- NA
  if (is.null(utmhem)) utmhem <- NA
  if (is.null(nx)) nx <- NA
  if (is.null(ny)) ny <- NA
  if (is.null(dgridkm)) dgridkm <- NA
  if (is.null(xorigkm)) xorigkm <- NA
  if (is.null(yorigkm)) yorigkm <- NA
  if (is.null(ibcomp)) ibcomp <- NA
  if (is.null(jbcomp)) jbcomp <- NA
  if (is.null(iecomp)) iecomp <- NA
  if (is.null(jecomp)) jecomp <- NA
  if (is.null(lsamp)) lsamp <- NA
  if (is.null(ibsamp)) ibsamp <- NA
  if (is.null(jbsamp)) jbsamp <- NA
  if (is.null(iesamp)) iesamp <- NA
  if (is.null(jesamp)) jesamp <- NA
  
  # Provide standard z heights if no vector provided
  if (is.null(zface)) zface = c(0,20,40,80,100,150,200,300,400,800,1400,2000,3000)
  
  # If option set to read x,y data from GEO.DAT file, get the relevant values
  if (read_xy_from_geo_dat == TRUE){
    
    # Generate vector list of all GEO.DAT files in the working folder
    geo_dat_file <- list.files(pattern = "geo--.*")
    
    # If there are multiple GEO.DAT files in the working folder, choose only
    # the first of the set
    if (length(geo_dat_file > 1)) geo_dat_file <- geo_dat_file[1]
    
    # Obtain several lines from the header portion of the GEO.DAT file
    geo_dat_header <- readLines(geo_dat_file, warn = FALSE)[
      (as.numeric(readLines(geo_dat_file, warn = FALSE)[2]) + 4):
      (as.numeric(readLines(geo_dat_file, warn = FALSE)[2]) + 6)]
    
    # Get the UTM zone and hemisphere
    iutmzn <- as.numeric(gsub("[ ]*([0-9]*).*", "\\1", geo_dat_header[1]))
    utmhem <- gsub("[ ]*[0-9]*([A-Z]*)[ ]*", "\\1", geo_dat_header[1])
    
    # Get the datum information
    datum <- gsub("([-A-Z0-9]*)[ ]*.*", "\\1", geo_dat_header[2])
    
  }
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("PMAP", "FEAST", "FNORTH", "IUTMZN", "UTMHEM", "RLAT0", "RLON0", 
                "XLAT1", "XLAT2", "DATUM", "NX", "NY", "DGRIDKM", "XORIGKM", "YORIGKM", 
                "NZ", "ZFACE", "IBCOMP", "JBCOMP", "IECOMP", "JECOMP", "LSAMP",
                "IBSAMP", "JBSAMP", "IESAMP", "JESAMP", "MESHDN")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(pmap, feast, fnorth, iutmzn, utmhem, rlat0, rlon0, 
                    xlat1, xlat2, datum, nx, ny, dgridkm, xorigkm, yorigkm, 
                    nz, zface, ibcomp, jbcomp, iecomp, jecomp, lsamp,
                    ibsamp, jbsamp, iesamp, jesamp, meshdn)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
}
