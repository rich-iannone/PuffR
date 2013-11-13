\name{surf.dat.generate}
\alias{surf.dat.generate}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%- maybe also 'usage' for other objects documented here.
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
Richard Iannone
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Generate "surf.dat" from previously defined period
surf.dat.generate()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
