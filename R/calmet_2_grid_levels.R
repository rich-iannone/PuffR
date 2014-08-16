2_calmet_grid_levels <- function(pmap = "UTM",
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
  
  if (!is.integer(nx) | !is.integer(ny)){
    stop("nx or ny must be integer values.")
  }  
  

}
