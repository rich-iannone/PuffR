\name{define.calmet.domain}
\alias{define.calmet.domain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
define.calmet.domain(lat.dec.deg = NULL, long.dec.deg = NULL, lat.long.grid.loc = 1, EPSG_code = NULL, projection = "UTM", datum = "WGS84", ellipse = "WGS84", units = "m", cell.resolution.m = 250, domain.width.m = NULL, domain.height.m = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat.dec.deg}{
%%     ~~Describe \code{lat.dec.deg} here~~
}
  \item{long.dec.deg}{
%%     ~~Describe \code{long.dec.deg} here~~
}
  \item{lat.long.grid.loc}{
%%     ~~Describe \code{lat.long.grid.loc} here~~
}
  \item{EPSG_code}{
%%     ~~Describe \code{EPSG_code} here~~
}
  \item{projection}{
%%     ~~Describe \code{projection} here~~
}
  \item{datum}{
%%     ~~Describe \code{datum} here~~
}
  \item{ellipse}{
%%     ~~Describe \code{ellipse} here~~
}
  \item{units}{
%%     ~~Describe \code{units} here~~
}
  \item{cell.resolution.m}{
%%     ~~Describe \code{cell.resolution.m} here~~
}
  \item{domain.width.m}{
%%     ~~Describe \code{domain.width.m} here~~
}
  \item{domain.height.m}{
%%     ~~Describe \code{domain.height.m} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (lat.dec.deg = NULL, long.dec.deg = NULL, lat.long.grid.loc = 1, 
    EPSG_code = NULL, projection = "UTM", datum = "WGS84", ellipse = "WGS84", 
    units = "m", cell.resolution.m = 250, domain.width.m = NULL, 
    domain.height.m = NULL) 
{
    require(rgdal)
    require(plyr)
    lat_long_dec_deg <- cbind(long.dec.deg, lat.dec.deg)
    UTM_zone <- (floor((long.dec.deg + 180)/6)\%\%60) + 1
    EPSG_string <- subset(EPSG_proj4, EPSG_proj4$code == EPSG_code)
    EPSG_string <- EPSG_string$proj4_string
    proj_string <- paste("+init=epsg:", EPSG_code, sep = "")
    proj_string_longlat <- c("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    UTM_location <- project(lat_long_dec_deg, proj_string)
    UTM_location <- round_any(UTM_location, 50, round)
    is_number_cells_across_x_an_int <- ifelse(domain.width.m\%\%cell.resolution.m != 
        0, FALSE, TRUE)
    is_number_cells_across_y_an_int <- ifelse(domain.height.m\%\%cell.resolution.m != 
        0, FALSE, TRUE)
    number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == 
        TRUE, domain.width.m/cell.resolution.m, NULL)
    number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == 
        TRUE, domain.height.m/cell.resolution.m, NULL)
    total_cells <- number_cells_across_x * number_cells_across_y
    left_UTM <- if (lat.long.grid.loc == 1) {
        UTM_location[1, 1] - (0.5 * domain.width.m)
    }
    else if (lat.long.grid.loc == 2) {
        UTM_location[1, 1]
    }
    else if (lat.long.grid.loc == 3) {
        UTM_location[1, 1] - domain.width.m
    }
    else if (lat.long.grid.loc == 4) {
        UTM_location[1, 1]
    }
    else if (lat.long.grid.loc == 5) {
        UTM_location[1, 1] - domain.width.m
    }
    else {
        NULL
    }
    right_UTM <- if (lat.long.grid.loc == 1) {
        UTM_location[1, 1] + (0.5 * domain.width.m)
    }
    else if (lat.long.grid.loc == 2) {
        UTM_location[1, 1] + domain.width.m
    }
    else if (lat.long.grid.loc == 3) {
        UTM_location[1, 1]
    }
    else if (lat.long.grid.loc == 4) {
        UTM_location[1, 1] + domain.width.m
    }
    else if (lat.long.grid.loc == 5) {
        UTM_location[1, 1]
    }
    else {
        NULL
    }
    bottom_UTM <- if (lat.long.grid.loc == 1) {
        UTM_location[1, 2] - (0.5 * domain.height.m)
    }
    else if (lat.long.grid.loc == 2) {
        UTM_location[1, 2]
    }
    else if (lat.long.grid.loc == 3) {
        UTM_location[1, 2]
    }
    else if (lat.long.grid.loc == 4) {
        UTM_location[1, 2] - domain.height.m
    }
    else if (lat.long.grid.loc == 5) {
        UTM_location[1, 2] - domain.height.m
    }
    else {
        NULL
    }
    top_UTM <- if (lat.long.grid.loc == 1) {
        UTM_location[1, 2] + (0.5 * domain.height.m)
    }
    else if (lat.long.grid.loc == 2) {
        UTM_location[1, 2] + domain.height.m
    }
    else if (lat.long.grid.loc == 3) {
        UTM_location[1, 2] + domain.height.m
    }
    else if (lat.long.grid.loc == 4) {
        UTM_location[1, 2]
    }
    else if (lat.long.grid.loc == 5) {
        UTM_location[1, 2]
    }
    else {
        NULL
    }
    LL_LR_UL_UR_m <- data.frame(x = c(left_UTM, right_UTM, left_UTM, 
        right_UTM), y = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
    LL_LR_UL_UR_UTM_m <- SpatialPoints(as.matrix(LL_LR_UL_UR_m), 
        proj4string = CRS(paste("+init=epsg:", EPSG_code, sep = "")))
    LL_LR_UL_UR_UTM_longlat <- spTransform(LL_LR_UL_UR_UTM_m, 
        CRS("+proj=longlat +ellps=GRS80"))
    latlong_bbox <- mat.or.vec(4, 1)
    latlong_bbox[1] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1, 
        1]
    latlong_bbox[2] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[1, 
        2]
    latlong_bbox[3] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2, 
        2]
    latlong_bbox[4] <- summary(LL_LR_UL_UR_UTM_longlat)$bbox[2, 
        1]
    return(latlong_bbox)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
