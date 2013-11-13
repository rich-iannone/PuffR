\name{ncdc.station.data}
\alias{ncdc.station.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain NCDC station data for all stations residing in the CALMET domain during a specied time period.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ncdc.station.data(startyear = NULL,
                  endyear = NULL,
                  use.calmet.bounds = TRUE,
                  manual.bounds = NULL)
}
%- maybe also 'usage' for other objects documented here.
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
# Get 2010 station data for a previously defined domain
ncdc.station.data(startyear = 2010,
                  endyear = 2010,
                  use.calmet.bounds = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
