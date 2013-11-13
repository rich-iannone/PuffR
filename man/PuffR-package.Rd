\name{PuffR-package}
\alias{PuffR-package}
\alias{PuffR}
\docType{package}
\title{
A collection of tools to help with dispersion modelling using CALPUFF
~~ PuffR ~~
}
\description{
This package allows for the discovery, retrieval, organization, and production of inputs for the CALPUFF model.  
}
\details{
\tabular{ll}{
Package: \tab PuffR\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-11-12\cr
License: \tab MIT\cr
}

}
\author{
Richard Iannone

Maintainer: Richard Iannone <riannone@me.com>

}
\references{

}

\keyword{ package }
\seealso{

}
\examples{
# Create a CALMET domain of 100 by 100 km
# in the Los Angeles area
define.calmet.domain(lat.dec.deg = 34.050184,
                    long.dec.deg = -118.253959,
                    lat.long.grid.loc = 1,
                    EPSG_code = 32611,
                    cell.resolution.m = 250,
                    domain.width.m = 100000,
                    domain.height.m = 100000)

# Get surface station data from NCDC
ncdc.station.data(startyear = 2010,
                  endyear = 2010,
                  use.calmet.bounds = TRUE)
                  
# Select only 3 stations from the retrieved
# stations in the domain
# The ormat for each station is [USAF]-[WBAN]
station.select(id = c("720046-99999",
                      "722950-23174",
                      "722886-23130"))

# Create a 'SURF.DAT' file for CALMET
surf.dat.generate()
}
