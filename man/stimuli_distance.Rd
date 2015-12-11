\name{stimuli_distance}
\alias{stimuli_distance}
\title{
combine data from observations and stimuli by distance
}
\description{
The function selects stimuli (e.g. disturbance events) prior to observational data by distance. The stimuli that falls within a specified timeframe, as well as the most recent stimulus and the upcoming stimulus for each observation (within the specified spatial range) will be combined. Additionally the output contains a dataframe for the observations and the stimuli with additional frequencies of the combinations of the data. The input data should be preformatted (see 'Arguments').}
\usage{
stimuli_distance(df_observed, df_stimuli, minutesbeforeobservation = 0, minutesafterobservation = 0, maxdist = FALSE, distance_algorithm = geosphere::distVincentyEllipsoid)
}
\arguments{
  \item{df_observed}{
A data.frame object of observation-data. The first two columns needs to contain longitudinal and latitudinal data. The third column needs to contain POSIXct-data. Additional columns are optional.
}
  \item{df_stimuli}{
A data.frame object of stimuli-data. The first two columns needs to contain longitudinal and latitudinal data. The third column needs to contain POSIXct-data. Additional columns are optional.
}
  \item{minutesbeforeobservation}{
Numerical value specifying the minutes before an observation. Only stimuli that falls within this timeframe are used for further processing. If 'minutesbeforeobservation' and 'minutesafterobservation' are equal to zero all possible combinations will be used (regardless of the time between the data). Default is 0.
}
  \item{minutesafterobservation}{
Numerical value specifying the minutes after an observation. Only stimuli that falls within this timeframe are used for further processing. If 'minutesbeforeobservation' and 'minutesafterobservation' are equal to zero all possible combinations will be used (regardless of the time between the data). Default is 0.
}
  \item{maxdist}{
Numerical value specifying the maximum distance between observational and stimuli data (in meter).
}
  \item{distance_algorithm}{
Specifies the algorithm used for the distance calculation. The function uses the 'distm'-function of the package 'geosphere'. Possible choices are 'geosphere::distVincentyEllipsoid' (default), 'geosphere::distVincentySphere', 'geosphere::distCosine' and 'geosphere::distHaversine'. See ?geosphere::distm for more information.
}
}
\value{
The output is a list object containing five data frames.
\item{timeframe }{A data frame with combinations of observed data and stimuli data that falls within the specified timeframe and spatial extent. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data.frame contains information about the distance (in meter) and the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{upcoming_stim }{A data frame with combinations of observed data and the upcoming stimuli data (regardless of the specified timeframe) that falls within the specified spatial extent. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data.frame contains information about the distance (in meter) and the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{most_recent_stim }{A data frame with combinations of observed data and the most recent stimuli data (regardless of the specified timeframe) that falls within the specified spatial extent. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data. frame contains information about the distance (in meter) and the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{frequency_observations }{A data.frame containing the input data frame 'df_observed' with additional information about the frequencies of the data used for the three data frames 'timeframe', 'upcoming_stim' and 'most_recent_stim'.}
\item{frequency_stimuli }{A data.frame containing the input data frame 'df_stimuli' with additional information about the frequencies of the data used for the three data frames 'timeframe', 'upcoming_stim' and 'most_recent_stim'.}
}
\references{
Robert J. Hijmans (2015). geosphere: Spherical Trigonometry. R package version 1.4-3. http://CRAN.R-project.org/package=geosphere
}
\author{
Frederik Sachser
}
\seealso{
\code{\link{stimuli_location}}
}
\examples{
  now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
  date_obs <- seq(now, now + 90*60*99, 90*60)
  date_stim <- sample(date_obs, 15) + sample(1:100, 1)
  attributes(date_obs)$tz <- 'CET'
  attributes(date_stim)$tz <- 'CET'
  lat_obs <- runif(n = 100, min = 46.1, max = 46.9)
  lon_obs <- runif(n = 100, min = 13.1, max = 13.9)
  lat_stim <- sample(lat_obs, 15) + 0.004
  lon_stim <- sample(lon_obs, 15) + 0.004
  df_obs <- data.frame(lon_obs, lat_obs, date_obs)
  df_stim <- data.frame(lon_stim, lat_stim, date_stim)
  stimuli_distance(df_observed = df_obs,
                   df_stimuli = df_stim,
                   minutesbeforeobservation = 480,
                   minutesafterobservation = 360,
                   maxdist = 1000,
                   distance_algorithm = geosphere::distVincentyEllipsoid)
}
\keyword{ stimuli_distance }
