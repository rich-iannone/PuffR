\name{Define the CALMET domain}
\alias{define.calmet.domain}
\title{
Define the CALMET domain
}

\description{
This function allows for the definition of the CALMET domain. The position and size of the domain are set in both lat/long and UTM coordinates. The number and size of the contained grid cells are also set with this function.
}

\usage{
define.calmet.domain(lat.dec.deg = NULL,
                     long.dec.deg = NULL,
                     lat.long.grid.loc = 1,
                     EPSG_code = NULL,
                     projection = "UTM",
                     datum = "WGS84",
                     ellipse = "WGS84",
                     units = "m",
                     cell.resolution.m = 250,
                     domain.width.m = NULL,
                     domain.height.m = NULL)
}

\arguments{
  \item{lat.dec.deg}{
the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat.long.grid.loc argument.
}
  \item{long.dec.deg}{
the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat.long.grid.loc argument.
}
  \item{lat.long.grid.loc}{
the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
}
  \item{EPSG_code}{
an EPSG code allows for the lookup of an appropriate PROJ.4 string. Information on EPSG codes is available on this page: http://cicero.azavea.com/docs/epsg_codes.html.
}
  \item{projection}{
the alternate projection of the domain for CALMET. Default is "UTM".
}
  \item{datum}{
the alternate datum of the domain for CALMET. Default is "WGS84".
}
  \item{ellipse}{
the alternate ellipse of the domain for CALMET. Default is "WGS84".
}
  \item{units}{
the alternate units of the domain for CALMET. Default is "m".
}
  \item{cell.resolution.m}{
the desired height and width of each square cell in the meteorological domain.
}
  \item{domain.width.m}{
the desired width of the meteorological domain in meters.
}
  \item{domain.height.m}{
the desired height of the meteorological domain in meters.
}
}

\details{

}

\value{

}

\references{

}

\author{
Richard Iannone
}

\note{

}


\seealso{

}

\examples{
# Create a CALMET domain of 100 by 100 km in the Los Angeles area.
# Chosen lat/long coordinates are for the center of the domain. 
define.calmet.domain(lat.dec.deg = 34.050184,
                    long.dec.deg = -118.253959,
                    lat.long.grid.loc = 1,
                    EPSG_code = 32611,
                    projection = "UTM",
                    datum = "WGS84",
                    ellipse = "WGS84",
                    units = "m",
                    cell.resolution.m = 250,
                    domain.width.m = 100000,
                    domain.height.m = 100000)
}
