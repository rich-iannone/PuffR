\name{surf.dat.generate}
\alias{surf.dat.generate}
\title{
Generate a SURF.DAT input file for CALMET
}

\description{
This function generates the SURF.DAT input file for use with the CALMET model.
}

\usage{
surf.dat.generate(startyear = NOAA_start_year,
                  endyear = NOAA_end_year,
                  outputfile = "surf.dat")
}

\arguments{
  \item{startyear}{
the starting year for the SURF.DAT file.
}
  \item{endyear}{
the ending year for the SURF.DAT file.
}
  \item{outputfile}{
the specified filename for the SURF.DAT file. Defaults to "surf.dat".
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
# Generate "surf.dat" from previously defined period

surf.dat.generate()
}
