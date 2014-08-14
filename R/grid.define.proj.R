2_define_proj_levels <- function(pmap = "UTM",
                                 feast = 0.0, fnorth = 0.0,
                                 iutmzn, utmhem = "N",
                                 rlat0, rlon0,
                                 xlat1, xlat2,
                                 datum, nx, ny,
                                 dgridkm, xorigkm, yorigkm,
                                 nz, zface){
  
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
  

}
