calmet_landuse_data <- function(){
  
  setwd("~/Downloads")
  
  # Add require statements
  require(MODISTools)
  
  GetProducts()
  GetBands(Product = "MCD12Q1")
  
  modis.subset <- data.frame(lat = 49.5, long = -127)
  modis.subset$start.date <- rep(2003, nrow(modis.subset))
  modis.subset$end.date <- rep(2004, nrow(modis.subset))
  
  GetDates(Product = "MCD12Q1", Lat = modis.subset$lat[1], Long = modis.subset$long[1])
  
  modis.subset$start.date <- rep(2003, nrow(modis.subset))
  modis.subset$end.date <- rep(2006, nrow(modis.subset))
  
  MODISSubsets(LoadDat = modis.subset, Products = "MCD12Q1",
               Bands = "Land_Cover_Type_1",
               Size = c(1,1))
  
  MODISSubsets(LoadDat = SubsetExample, 
               Products = "MOD13Q1",
               Bands = c("250m_16_days_EVI",
                         "250m_16_days_pixel_reliability"),
               Size = c(0,0), StartDate = TRUE)
  
  
  MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1",
               Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"),
               Size = c(1,1))
  
  LandCover(Band = "Land_Cover_Type_1")
  
  
  
  data(SubsetExample)
  
  
  MODISSubsets(LoadDat = SubsetExample, Products = "MOD13Q1", 
               Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"), Size = c(1,1), 
               StartDate = FALSE, TimeSeriesLength = 1)
  
  MODISSummaries(LoadDat = SubsetExample, Product = "MOD13Q1", Bands = "250m_16_days_EVI", 
                 ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
                 StartDate = FALSE, QualityScreen = TRUE, QualityThreshold = 0,
                 QualityBand = "250m_16_days_pixel_reliability")
  
  if(sum(grepl("MODIS_Data", list.files())) != 1){
    print("Could not identify 'MODIS_Data' csv output file from MODISSummaries")
  } else {
    TileExample <- read.csv(list.files(pattern = "MODIS_Data"))
    TileExample <- TileExample[1,which(grepl("pixel", names(TileExample)))]
    
    dim(TileExample)
    dim(ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = FALSE))
    ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = FALSE)
    
    matrix(TileExample, nrow = 9, ncol = 9, byrow = TRUE)
    ExtractTile(Data = TileExample, Rows = c(9,2), Cols = c(9,2), Grid = TRUE)
  }
  
}
