\name{circadian_timeframe}
\alias{circadian_timeframe}
\title{
Classify POSIXct by timeframe
}
\description{
The function classifies objects of class POSIXct whether it falls within the specified timeframe or not.
}
\usage{
circadian_timeframe(data_POSIXct, begin = "00:00", end = "00:00")
}
\arguments{
  \item{data_POSIXct}{
  An object of class POSIXct.
}
  \item{begin}{
A character value indicating the offset of the timeframe. Format should be "hh:mm" (24-hour clock system). Default is "00:00".
}
  \item{end}{
A character value indicating the end time of the timeframe. Format should be "hh:mm" (24-hour clock system). Default is "00:00".
}
}
\details{
It is not possible to specify a timeframe over a period of >1 day (e.g. from "22:00"-"08:00"). If this is required a simple workaround would be to split the timeframe, rerun the function and combine the output. Furthermore it is important to make sure that the "begin" is before the "end" because there will be no warning but a wrong output.
}
\value{
The output is a data.frame object with only one column. The column name depends on the input of the timeframe (eg "T13.00_T14.00" indicates begin = "13:00" and end = "14:00"). If desired you can use cbind() to add this information to the original dataframe.
}
\author{
Frederik Sachser
}
\seealso{
\code{\link{twilight_timeframe}}
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
art_date <- seq(now, now + 90*60*99, 90*60)
attributes(art_date)$tz <- 'CET'
circadian_timeframe(data_POSIXct = art_date, begin = "12:00", end = "15:30")
}
\keyword{circadian_timeframe}
