\name{get.ncdc.history}
\alias{get.ncdc.history}
\title{
Retrieve the NCDC history data file.
}

\description{
This function opens a connection to the NCDC FTP server for downloading of the surface station history file. It is not essential to use this function in a normal workflow since the 'ncdc.station.data' function will normally retrieve this file during its operation. This function is useful if just the file is desired for inspection.
}

\usage{
get.ncdc.history(replace.file = FALSE)
}

\arguments{
  \item{replace.file}{
selecting 'yes' will overwrite history file if it exists in the working directory.
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
# Obtain the NCDC history file

get.ncdc.history()
}
