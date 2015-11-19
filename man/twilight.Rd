\name{twilight}
\alias{twilight}
\title{
calculate sunrise and sunset (nautic, civil, rise/set)
}
\description{
The function calculates sunrise and sunset of specified coordinates in POSIXct-format. You can choose out of three different twilight conditions: "civil", "nautic" and "rise/set".
}
\usage{
twilight(data_POSIXct, latitude, longitude, twilight = "civil")
}
\arguments{
  \item{data_POSIXct}{
An object of class POSIXct. It is important to specify the correct timezone.
}
  \item{latitude}{
specify latitude
}
  \item{longitude}{
specify longitude
}
  \item{twilight}{
A character value: Choose between "civil", "nautic" or "rise/set" to specify required twilight condition. Default: "civil"
}
}
\value{
The output is a data.frame object containing 2 columns (sunrise and sunset in POSIXct-format). If desired you can use cbind() to add this information to the original dataframe.
}
\references{
The formula was adapted from the following webpage:
http://lexikon.astronomie.info/zeitgleichung/
}
\author{
Thomas Ruf; Frederik Sachser
}
\note{
The function is invoked by running twilight_timeframe().
}
\seealso{
\code{\link{twilight_timeframe}}
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
art_date <- seq(now, now + 90*60*99, 90*60)
attributes(art_date)$tz <- 'CET'
lat <- runif(n = 100, min = 46.1, max = 46.9)
lon <- runif(n = 100, min = 13.1, max = 13.9)
twilight(data_POSIXct = art_date, latitude = lat, longitude = lon, twilight = "civil")
}
\keyword{twilight}
