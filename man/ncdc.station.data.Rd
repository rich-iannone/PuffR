\name{ncdc.station.data}
\alias{ncdc.station.data}
\title{
Obtain NCDC station data for all stations residing in the CALMET domain during a specied time period.
}

\description{
This function allows you to get NCDC station data for a specified time frame using either a previously defined set of boundaries (i.e., a CALMET domain) or by specifying a bounding box.
}

\usage{
ncdc.station.data(startyear = NULL,
                  endyear = NULL,
                  use.calmet.bounds = TRUE,
                  manual.bounds = NULL)
}

\arguments{
  \item{startyear}{
the year for the start of data retrieval.
}
  \item{endyear}{
the year for the end of data retrieval.
}
  \item{use.calmet.bounds}{
a 'TRUE' or 'FALSE' option on whether to use the same bounds as the previously defined CALMET domain. 
}
  \item{manual.bounds}{
option to specify boundaries using lat/long coordinates in decimal degrees.
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
# Get 2010 station data for a previously defined domain
ncdc.station.data(startyear = 2010,
                  endyear = 2010,
                  use.calmet.bounds = TRUE)
}
