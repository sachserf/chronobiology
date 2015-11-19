\name{POSIXct_info}
\alias{POSIXct_info}
\title{
Wrapper function to get additional information from POSIXct objects
}
\description{
The function is a wrapper to get additional information from a POSIXct object. It is straightforward to subset the original data based on the output.
}
\usage{
POSIXct_info(data_POSIXct)
}
\arguments{
  \item{data_POSIXct}{
    An object of class POSIXct. It is important to specify the correct timezone.
}
}
\value{
The output is a data.frame object including many additional information based on the input. If desired you can use cbind() to add this information to the original dataframe.
}
\author{
Frederik Sachser
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
art_date <- seq(now, now + 90*60*99, 90*60)
attributes(art_date)$tz <- 'CET'
POSIXct_info(data_POSIXct = art_date)
}
\keyword{POSIXct_info}
