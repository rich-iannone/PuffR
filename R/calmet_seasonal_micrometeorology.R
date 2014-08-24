calmet_seasonal_micrometeorology <- function(){
  
  # Winter
  LU_categories <- seq(10, 80, 10)
  z0 <- c(0.5, 0.01, 0.01, 1.1, 0.005, 0.5, 0.05, 0.05)
  albedo <- c(0.25, 0.4, 0.4, 0.15, 0.5, 0.4, 0.4, 0.4)
  BR <- c(0.25, 0.4, 0.4, 0.15, 0.5, 0.4, 0.4, 0.4)
  HCG <- c(0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15)
  QF <- c(0.4, 0, 0, 0, 0, 0, 0, 0)
  LAI <- c(0.5, 1.5, 0.5, 4, 0, 1, 0.05, 0.05)
  season <- rep("Winter", length(LU_categories))
  
  winter <- data.frame(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  rm(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  # Spring
  LU_categories <- seq(10, 80, 10)
  z0 <- c(0.52, 0.05, 0.03, 1.15, 0.001, 0.7, 0.05, 0.05)
  albedo <- c(0.1, 0.18, 0.14, 0.11, 0.08, 0.11, 0.18, 0.18)
  BR <- c(0.8, 0.3, 0.3, 0.7, 0.1, 0.2, 1.5, 1.5)
  HCG <- c(0.2, 0.15, 0.15, 0.15, 1, 0.25, 0.15, 0.15)
  QF <- c(0.1, 0, 0, 0, 0, 0, 0, 0)
  LAI <- c(0.7, 2.5, 0.5, 6, 0, 2, 0.05, 0.05)
  season <- rep("Spring", length(LU_categories))
  
  spring <- data.frame(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  rm(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  # Summer
  LU_categories <- seq(10, 80, 10)
  z0 <- c(0.54, 0.1, 0.15, 1.3, 0.001, 0.7, 0.05, 0.05)
  albedo <- c(0.04, 0.14, 0.14, 0.04, 0.03, 0.04, 0.15, 0.15)
  BR <- c(0.8, 0.7, 0.8, 0.4, 0.1, 0.2, 1.4, 1.4)
  HCG <- c(0.2, 0.15, 0.15, 0.15, 1, 0.25, 0.15, 0.15)
  QF <- c(0, 0, 0, 0, 0, 0, 0, 0)
  LAI <- c(0.8, 1.5, 0.5, 5, 0, 2, 0.05, 0.05)
  season <- rep("Summer", length(LU_categories))
  
  summer <- data.frame(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  rm(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  # Fall
  LU_categories <- seq(10, 80, 10)
  z0 <- c(0.54, 0.1, 0.15, 1.3, 0.001, 0.7, 0.05, 0.05)
  albedo <- c(0.05, 0.06, 0.06, 0.04, 0.04, 0.04, 0.08, 0.08)
  BR <- c(0.8, 0.7, 0.8, 0.4, 0.1, 0.2, 1.4, 1.4)
  HCG <- c(0.2, 0.15, 0.15, 0.15, 1, 0.25, 0.15, 0.15)
  QF <- c(0.1, 0, 0, 0, 0, 0, 0, 0)
  LAI <- c(0.7, 0.15, 0.15, 0.15, 1, 0.25, 0.15, 0.15)
  season <- rep("Fall", length(LU_categories))
  
  fall <- data.frame(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  rm(LU_categories, z0, albedo, BR, HCG, QF, LAI, season)
  
  # Bind all of the seasonal data frames together
  mmet_seasons <- rbind(rbind(rbind(winter, spring), summer), fall)
  
  return(mmet_seasons)
  
}