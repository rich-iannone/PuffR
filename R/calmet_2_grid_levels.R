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
#' @export calmet_2_grid_levels

calmet_2_grid_levels <- function(calmet_inp,
                                 pmap = "UTM",
                                 feast = 0.0, fnorth = 0.0,
                                 iutmzn, utmhem = "N",
                                 rlat0 = "40N", rlon0 = "90W",
                                 xlat1 = "30N", xlat2 = "60N",
                                 datum = "WGS-84", nx, ny,
                                 dgridkm, xorigkm, yorigkm,
                                 nz = 12,
                                 zface = c(0,20,40,80,100,150,200,300,400,800,1400,2000,3000)){
  
  # Define the valid grid projections
  possible_projections <- c("UTM", "TTM", "LCC", "PS", "EM", "LAZA")
  
  # Stop function of pmap not part of 'possible_projections' set
  if (!(pmap %in% possible_projections)){
    
    stop("The chosen projection is not valid.")
  
  }
  
  # If pmap either of TTM, LCC, or LAZA, a false easting and false northing
  # are required
  if (pmap %in% c("TTM", 'LCC', "LAZA")){
    
  }
  
  # Validate use of UTM zone
  if (pmap == "UTM"){
    
    if (!(iutmzn %in% seq(1, 60, 1))){
      
      stop("The UTM zone must be an integer from 1 to 60")
      
    }
    
  }
  
  # Validate use of hemisphere for UTM zone
  if (pmap == "UTM"){
    
    if (!(iutmzn %in% c("N", "S"))){
      
      stop("The UTM zone must either be in the northern (N) or southern (S) hemisphere.")
      
    }
    
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

}
