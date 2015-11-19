\name{custom_cycle}
\alias{custom_cycle}
\title{
Create a custom cycle and compare with data
}
\description{
The function creates a cycle according to user-specified input. It is possible to define one or two parts (e.g. 12.4 hours followed by 5 hours and so on).Furthermore the function produces information of the iteration count and the part of the cycle (called A and B) according to the input data.
}
\usage{
custom_cycle(data_POSIXct, starting_point = NULL, hours_part_A, hours_part_B = 0)
}
\arguments{
  \item{data_POSIXct}{
An object of class POSIXct.

}
  \item{starting_point}{
Optional object of class POSIXct. The default is to start the first cycle at the same time of the earliest data_POSIXct object. A custom starting_point should be prior to the earliest data_POSIXct object.
}
  \item{hours_part_A}{
Required. A positive number indicating the hours of the first part of the cycle.
}
  \item{hours_part_B}{
Optional. A positive number indicating the hours of the second part of the cycle. Default is 0.
}
}
\value{
The output is a data.frame object including information about the starting points, iteration count and part of the cycle according to each sample (data_POSIXct).
}
\author{
Frederik Sachser
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
art_date <- seq(now, now + 90*60*99, 90*60)
custom_cycle(data_POSIXct = art_date, starting_point = now, hours_part_A = 12.5, hours_part_B = 5)
}
\keyword{ custom_cycle }
