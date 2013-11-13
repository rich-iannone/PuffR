\name{ncdc.station.data}
\alias{ncdc.station.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ncdc.station.data(startyear = NULL, endyear = NULL, use.calmet.bounds = TRUE, manual.bounds = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startyear}{
%%     ~~Describe \code{startyear} here~~
}
  \item{endyear}{
%%     ~~Describe \code{endyear} here~~
}
  \item{use.calmet.bounds}{
%%     ~~Describe \code{use.calmet.bounds} here~~
}
  \item{manual.bounds}{
%%     ~~Describe \code{manual.bounds} here~~
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
function (startyear = NULL, endyear = NULL, use.calmet.bounds = TRUE, 
    manual.bounds = NULL) 
{
    require(lubridate)
    require(plyr)
    require(stringr)
    use.manual.bounds <- FALSE
    if (is.null(startyear) | is.null(endyear)) {
        stop("Please enter starting and ending years for surface station data")
    }
    else {
    }
    if (!is.numeric(startyear) | !is.numeric(endyear)) {
        stop("Please enter numeric values for the starting and ending years")
    }
    else {
    }
    if (startyear > endyear) {
        stop("Please enter the starting and ending years in the correct order")
    }
    else {
    }
    if (startyear < 1892 | endyear < 1892 | startyear > year(Sys.Date()) | 
        endyear > year(Sys.Date())) {
        stop("Please enter the starting and ending years in the correct order")
    }
    else {
    }
    if (!file.exists("define.calmet.domain.out")) {
        stop("The CALMET domain hasn't been defined. Please define domain theses bounds by:\n         (1) entering manually: ncdc.station.data(startyear = [start year],\n                                                  endyear = [end year],\n                                                  manual.bounds = c([W], [E], [N], [S])\n         (2) using the 'define.calmet.domain' function")
    }
    else {
    }
    if (!is.null(manual.bounds)) {
        if (!is.numeric(manual.bounds[1]) | !is.numeric(manual.bounds[2]) | 
            !is.numeric(manual.bounds[3]) | !is.numeric(manual.bounds[4]) | 
            length(manual.bounds) != 4) {
            stop("Please define manual bounds as numeric vector in the order: W, E, N, S")
        }
        else {
            use.manual.bounds <- TRUE
        }
    }
    NOAA_start_year <- startyear
    NOAA_end_year <- endyear
    get.ncdc.history()
    st <- read.csv("ish-history.csv")
    names(st)[c(3, 10)] <- c("NAME", "ELEV")
    st <- st[, -5]
    st$LAT <- st$LAT/1000
    st$LON <- st$LON/1000
    st$ELEV <- st$ELEV/10
    st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
    st$END <- as.numeric(substr(st$END, 1, 4))
    if (use.manual.bounds == TRUE) {
        domain.list <- subset(st, st$LON >= manual.bounds[1] & 
            st$LON <= manual.bounds[2] & st$LAT >= manual.bounds[3] & 
            st$LAT <= manual.bounds[4] & BEGIN <= NOAA_start_year & 
            END >= NOAA_end_year)
    }
    else if (use.calmet.bounds == TRUE) {
        domain.list <- subset(st, st$LON >= as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[1] & 
            st$LON <= as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[2] & 
            st$LAT <= as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[3] & 
            st$LAT >= as.numeric(unlist(read.table(file = "define.calmet.domain.out")))[4] & 
            BEGIN <= NOAA_start_year & END >= NOAA_end_year)
    }
    outputs <- as.data.frame(matrix(NA, dim(domain.list)[1], 
        2))
    names(outputs) <- c("FILE", "STATUS")
    for (y in NOAA_start_year:NOAA_end_year) {
        y.domain.list <- domain.list[domain.list$BEGIN <= y & 
            domain.list$END >= y, ]
        for (s in 1:dim(y.domain.list)[1]) {
            outputs[s, 1] <- paste(sprintf("\%06d", y.domain.list[s, 
                1]), "-", sprintf("\%05d", y.domain.list[s, 2]), 
                "-", y, ".gz", sep = "")
            system(paste("curl -O ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", 
                y, "/", outputs[s, 1], sep = ""))
            outputs[s, 2] <- ifelse(file.exists(outputs[s, 1]) == 
                "TRUE", "available", "missing")
        }
    }
    file_report <- cbind(y.domain.list, outputs)
    row.names(file_report) <- 1:nrow(file_report)
    system("gunzip *.gz", intern = FALSE, ignore.stderr = TRUE)
    files <- list.files(pattern = "^[0-9]*-[0-9]*-[0-9]*$")
    column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 
        5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 
        1, 5, 1)
    stations <- as.data.frame(matrix(NA, length(files), 6))
    names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", 
        "ELEV")
    for (i in 1:length(files)) {
        data <- read.fwf(files[i], column.widths)
        data <- data[, c(2:8, 10:11, 13, 16, 19, 21, 29, 31, 
            33)]
        names(data) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", 
            "MIN", "LAT", "LONG", "ELEV", "WIND.DIR", "WIND.SPD", 
            "CEIL.HGT", "TEMP", "DEW.POINT", "ATM.PRES")
        data$LAT <- data$LAT/1000
        data$LONG <- data$LONG/1000
        data$WIND.DIR <- ifelse(data$WIND.DIR == 999, 999, data$WIND.DIR)
        data$WIND.SPD <- ifelse(data$WIND.SPD > 100, 999.9, data$WIND.SPD/10)
        data$TEMP <- ifelse(data$TEMP > 900, 999.9, round((data$TEMP/10) + 
            273.2, 1))
        data$DEW.POINT <- ifelse(data$DEW.POINT > 100, 999.9, 
            data$DEW.POINT/10)
        data$ATM.PRES <- ifelse(data$ATM.PRES > 2000, 999.9, 
            data$ATM.PRES/10)
        data$CEIL.HGT <- ifelse(data$CEIL.HGT == 99999, 999.9, 
            round(data$CEIL.HGT * 3.28084/100, 0))
        additional.data <- as.data.frame(scan(files[i], what = "character", 
            sep = "\n"))
        colnames(additional.data) <- c("string")
        number_of_add_lines <- sum(str_detect(additional.data$string, 
            "ADD"), na.rm = TRUE)
        percentage_of_add_lines <- (number_of_add_lines/length(additional.data$string)) * 
            100
        number_of_precip_lines <- sum(str_detect(additional.data$string, 
            "AA1"), na.rm = TRUE)
        percentage_of_precip_lines <- (number_of_precip_lines/length(additional.data$string)) * 
            100
        if (number_of_precip_lines > 0) {
            AA1_precip_period_in_hours <- as.character(str_extract_all(additional.data$string, 
                "AA1[0-9][0-9]"))
            AA1_precip_period_in_hours <- str_replace_all(AA1_precip_period_in_hours, 
                "AA1([0-9][0-9])", "\\1")
            AA1_precip_period_in_hours <- as.numeric(AA1_precip_period_in_hours)
            AA1_precip_depth_in_mm <- as.character(str_extract_all(additional.data$string, 
                "AA1[0-9][0-9][0-9][0-9][0-9][0-9]"))
            AA1_precip_depth_in_mm <- str_replace_all(AA1_precip_depth_in_mm, 
                "AA1[0-9][0-9]([0-9][0-9][0-9][0-9])", "\\1")
            AA1_precip_depth_in_mm <- as.numeric(AA1_precip_depth_in_mm)/10
            AA1_precip_rate_in_mm_per_hour <- AA1_precip_depth_in_mm/AA1_precip_period_in_hours
            additional.data$PRECIP.RATE <- round_any(AA1_precip_rate_in_mm_per_hour, 
                0.1, f = round)
        }
        if (number_of_precip_lines == 0) {
            additional.data$PRECIP.RATE <- rep(NA, length(additional.data$string))
        }
        additional.data$string <- NULL
        data <- cbind(data, additional.data)
        RH <- ifelse(data$TEMP == 999.9 | data$DEW.POINT == 999.9, 
            NA, 100 * (exp((17.625 * data$DEW.POINT)/(243.04 + 
                data$DEW.POINT))/exp((17.625 * (data$TEMP - 273.2))/(243.04 + 
                (data$TEMP - 273.2)))))
        data$RH <- round_any(as.numeric(RH), 0.1, f = round)
        PRECIP.CODE <- with(data, ifelse(PRECIP.RATE > 0 & PRECIP.RATE < 
            2.5, 1, ifelse(PRECIP.RATE >= 2.5 & PRECIP.RATE < 
            7.6, 2, ifelse(PRECIP.RATE >= 7.6, 3, 9999))))
        PRECIP.CODE <- ifelse(PRECIP.CODE < 25 & data$TEMP < 
            273.2, PRECIP.CODE + 18, PRECIP.CODE)
        data$PRECIP.CODE <- PRECIP.CODE
        write.csv(data, file = paste(files[i], ".csv", sep = ""), 
            row.names = FALSE)
        stations[i, 1:3] <- data[1, 1:3]
        stations[i, 4:6] <- data[1, 8:10]
    }
    write.csv(stations, file = "stations.csv", row.names = FALSE)
    NOAA.years.out <- mat.or.vec(2, 1)
    NOAA.years.out[1] <- NOAA_start_year
    NOAA.years.out[2] <- NOAA_end_year
    write.table(NOAA.years.out, file = "NOAA.years.out", col.names = FALSE, 
        row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
