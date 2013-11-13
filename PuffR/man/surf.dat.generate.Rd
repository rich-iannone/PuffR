\name{surf.dat.generate}
\alias{surf.dat.generate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
surf.dat.generate(startyear = NOAA_start_year, endyear = NOAA_end_year, outputfile = "surf.dat")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startyear}{
%%     ~~Describe \code{startyear} here~~
}
  \item{endyear}{
%%     ~~Describe \code{endyear} here~~
}
  \item{outputfile}{
%%     ~~Describe \code{outputfile} here~~
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
function (startyear = NOAA_start_year, endyear = NOAA_end_year, 
    outputfile = "surf.dat") 
{
    require(lubridate)
    NOAA_start_year <- as.numeric(unlist(read.table(file = "NOAA.years.out")))[1]
    NOAA_end_year <- as.numeric(unlist(read.table(file = "NOAA.years.out")))[2]
    selected_synthetic_id <- read.csv("selected_synthetic_id.csv", 
        header = TRUE)
    start_time <- ISOdatetime(startyear, 1, 1, hour = 0, min = 0, 
        sec = 0, tz = "GMT")
    end_time <- ISOdatetime(endyear, 12, 31, hour = 24, min = 0, 
        sec = 0, tz = "GMT")
    time_difference <- difftime(end_time, start_time, units = "hours")
    total_hours <- time_difference[[1]]
    time_series <- as.list(c(1:total_hours))
    for (i in 1:total_hours) {
        time_series[[i]] <- start_time + (3600 * (i - 1))
    }
    station_data_frames <- as.list(c(1:nrow(selected_synthetic_id)))
    for (i in 1:nrow(selected_synthetic_id)) {
        station_data_frames[[i]] <- list(read.csv(paste(selected_synthetic_id$CSV[i]), 
            header = TRUE))
    }
    cat("", file = paste(outputfile))
    cat("SURF.DAT        2.1             Hour Start and End Times with Seconds", 
        file = paste(outputfile), sep = "\n", append = TRUE)
    cat("1", file = paste(outputfile), sep = "\n", append = TRUE)
    cat("Produced using R", file = paste(outputfile), sep = "\n", 
        append = TRUE)
    cat("NONE", file = paste(outputfile), sep = "\n", append = TRUE)
    cat("UTC-0800", file = paste(outputfile), sep = "\n", append = TRUE)
    cat(year(time_series[[1]]), "  ", yday(time_series[[1]]), 
        "  ", hour(time_series[[1]]), "  ", year(time_series[[total_hours]] + 
            3600), "  ", yday(time_series[[total_hours]] + 3600), 
        "  ", hour(time_series[[total_hours]] + 3600), "  ", 
        nrow(selected_synthetic_id), file = paste(outputfile), 
        sep = "", append = TRUE)
    cat("", file = paste(outputfile), sep = "\n", append = TRUE)
    for (i in 1:length(station_data_frames)) {
        cat(paste(as.character(i), as.character(i), as.character(i), 
            as.character(i), as.character(i), sep = ""), file = paste(outputfile), 
            sep = "\n", append = TRUE)
    }
    for (i in 1:total_hours) {
        cat(year(time_series[[i]]), "  ", yday(time_series[[i]]), 
            "  ", hour(time_series[[i]]), "  ", year(time_series[[i]] + 
                3600), "  ", yday(time_series[[i]] + 3600), "  ", 
            hour(time_series[[i]] + 3600), file = paste(outputfile), 
            append = TRUE)
        cat("", file = paste(outputfile), sep = "\n", append = TRUE)
        for (j in 1:length(station_data_frames)) {
            cat("  ", station_data_frames[[j]][[1]]$WIND.SPD[i], 
                "  ", station_data_frames[[j]][[1]]$WIND.DIR[i], 
                "  ", station_data_frames[[j]][[1]]$CEIL.HGT[i], 
                "  ", "  ", station_data_frames[[j]][[1]]$TEMP[i], 
                "  ", station_data_frames[[j]][[1]]$RH[i], "  ", 
                station_data_frames[[j]][[1]]$ATM.PRES[i], "  ", 
                station_data_frames[[j]][[1]]$PRECIP.CODE[i], 
                file = paste(outputfile), append = TRUE)
            cat("", file = paste(outputfile), sep = "\n", append = TRUE)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
