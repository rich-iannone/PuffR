\name{get.ncdc.history}
\alias{get.ncdc.history}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.ncdc.history(replace.file = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replace.file}{
%%     ~~Describe \code{replace.file} here~~
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
function (replace.file = FALSE) 
{
    file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
    if (replace.file == TRUE) {
        repeat {
            try(download.file(file, "ish-history.csv", quiet = TRUE))
            if (file.info("ish-history.csv")$size > 0) {
                break
            }
        }
    }
    else {
    }
    if (file.exists("ish-history.csv") & file.info("ish-history.csv")$size > 
        0 & replace.file == FALSE) {
    }
    else {
        repeat {
            try(download.file(file, "ish-history.csv", quiet = TRUE))
            if (file.info("ish-history.csv")$size > 0) {
                break
            }
        }
    }
    st <- read.csv("ish-history.csv")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
