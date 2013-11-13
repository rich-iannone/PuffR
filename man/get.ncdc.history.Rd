\name{get.ncdc.history}
\alias{get.ncdc.history}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve the NCDC history data file.
}
\description{
This function opens a connection to the NCDC FTP server for downloading of the surface station history file. It is not essential to use this function in a normal workflow since the 'ncdc.station.data' function will normally retrieve this file during its operation. This function is useful if just the file is desired for inspection.
}
\usage{
get.ncdc.history(replace.file = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replace.file}{
selecting 'yes' will overwrite history file if it exists in the working directory.
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
# Obtain the NCDC history file
get.ncdc.history
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
