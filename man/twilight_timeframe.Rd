\name{twilight_timeframe}
\alias{twilight_timeframe}
\title{
Specify timeframe around dusk and dawn
}
\description{
A wrapper function for twilight(). In addition to the calculation of sunrise and sunset the function compares the specified timeframe with current data (optional).
}
\usage{
twilight_timeframe(data_POSIXct, latitude, longitude, twilight = "civil", min2sunrise = 0, minaftersunrise = 0, min2sunset = 0, minaftersunset = 0)
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
  \item{min2sunrise}{
Positive numeric value indicating the minutes before sunrise.
}
  \item{minaftersunrise}{
Positive numeric value indicating the minutes after sunrise.
}
  \item{min2sunset}{
Positive numeric value indicating the minutes before sunset.
}
  \item{minaftersunset}{
Positive numeric value indicating the minutes after sunset.
}
}
\details{
By using the function it is straightforward to subset the original data and get additional information about lighting conditions.
}
\value{
The output is a data.frame object (including sunrise and sunset in POSIXct-format). If desired you can use cbind() to add this information to the original dataframe.
}
\references{
The formula within the function twilight() was adapted from the following webpage:
http://lexikon.astronomie.info/zeitgleichung/}
\author{
Frederik Sachser, Thomas Ruf
}
\seealso{
\code{\link{twilight}}
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
art_date <- seq(now, now + 90*60*99, 90*60)
attributes(art_date)$tz <- 'CET'
lat <- runif(n = 100, min = 46.1, max = 46.9)
lon <- runif(n = 100, min = 13.1, max = 13.9)
twilight_timeframe(data_POSIXct = art_date, latitude = lat, longitude = lon, twilight = "civil", min2sunrise = 90, minaftersunrise = 90, min2sunset = 90, minaftersunset = 90)
}
\keyword{twilight_timeframe}
