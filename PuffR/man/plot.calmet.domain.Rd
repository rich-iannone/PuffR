\name{plot.calmet.domain}
\alias{plot.calmet.domain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.calmet.domain()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    require(ggmap)
    require(raster)
    extent_in_lat_long <- extent(as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[1], 
        as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[2], 
        as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[4], 
        as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[3])
    mid_pt_lat <- (as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[3] + 
        as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[4])/2
    mid_pt_long <- (as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[1] + 
        as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[2])/2
    the_map <- get_map(location = c(mid_pt_long, mid_pt_lat), 
        zoom = 10, maptype = "roadmap")
    map <- ggmap(the_map) + geom_point(data = read.csv("stations.csv", 
        header = TRUE), aes(x = read.csv("stations.csv", header = TRUE)$LONG, 
        y = read.csv("stations.csv", header = TRUE)$LAT), size = 3) + 
        geom_text(data = read.csv("stations.csv", header = TRUE), 
            aes(x = read.csv("stations.csv", header = TRUE)$LONG + 
                0.005, y = read.csv("stations.csv", header = TRUE)$LAT, 
                label = USAFID, hjust = 0, vjust = 0), size = 3) + 
        coord_equal() + labs(x = "Longitude") + labs(y = "Latitude") + 
        labs(title = "Plot of Surface Stations in Meteorological Domain")
    map
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
