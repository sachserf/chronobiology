\name{stimuli_location}
\alias{stimuli_location}
\title{
combine data from observations and stimuli (by location)
}
\description{
The function selects stimuli (e.g. disturbance events) prior to observational data by location. The stimuli that falls within a specified timeframe, as well as the most recent stimulus and the upcoming stimulus for each observation will be combined (only coincident locations). Additionally the output contains a dataframe for the observations and the stimuli with additional frequencies of the combinations of the data. The input data should be preformatted (see 'Arguments').
}
\usage{
stimuli_location(df_observed, df_stimuli, minutesbeforeobservation = 0, minutesafterobservation = 0)
}
\arguments{
  \item{df_observed}{
A data.frame object of observation-data. The first column needs to contain an ID about the location. The second column needs to contain POSIXct-data. Additional columns are optional.
}
  \item{df_stimuli}{
A data.frame object of stimuli-data. The first column needs to contain an ID about the location. The second column needs to contain POSIXct-data. Additional columns are optional.
}
  \item{minutesbeforeobservation}{
Numerical value specifying the minutes before an observation. Only stimuli that falls within this timeframe are used for further processing. If 'minutesbeforeobservation' and 'minutesafterobservation' are equal to zero all possible combinations will be used (regardless of the time between the data). Default is 0.
}
  \item{minutesafterobservation}{
Numerical value specifying the minutes after an observation. Only stimuli that falls within this timeframe are used for further processing. If 'minutesbeforeobservation' and 'minutesafterobservation' are equal to zero all possible combinations will be used (regardless of the time between the data). Default is 0.
}
}
\details{
The location column is not optional. If theres only one location in your data simply use a repeated value.
}
\value{
The output is a list object containing five data frames.
\item{timeframe }{A data frame with combinations of observed data and stimuli data for coincident locations that falls within the specified timeframe. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data.frame contains information about the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{upcoming_stim }{A data frame with combinations of observed data and the upcoming stimuli data (regardless of the specified timeframe) for coincident locations. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data.frame contains information about the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{most_recent_stim }{A data frame with combinations of observed data and the most recent stimuli data (regardless of the specified timeframe) for coincident locations. The output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli. Additonally there are columns specifying the row names of the original data. Furthermore the data.frame contains information about the time between the observation and the stimuli (in minutes). A negative value of this time indicates that the stimulus was before the observation; a positive value indicates that the stimuli was later than the observation.}
\item{frequency_observations }{A data.frame containing the input data frame 'df_observed' with additional information about the frequencies of the data used for the three data frames 'timeframe', 'upcoming_stim' and 'most_recent_stim'.}
\item{frequency_stimuli }{A data.frame containing the input data frame 'df_stimuli' with additional information about the frequencies of the data used for the three data frames 'timeframe', 'upcoming_stim' and 'most_recent_stim'.}
}
\author{
Frederik Sachser
}
\seealso{
\code{\link{stimuli_distance}}
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
date_obs <- seq(now, now + 90*60*99, 90*60)
date_stim <- sample(date_obs, 15) + 60
loc_stim <- sample(c("A", "B", "C"), length(date_stim), replace = TRUE)
loc_obs <- sample(c("A", "B", "D"), length(date_obs), replace = TRUE)
df_obs <- data.frame(loc_obs, date_obs)
df_stim <- data.frame(loc_stim, date_stim)
df_stim
the_output <- stimuli_location(df_observed = df_obs, df_stimuli = df_stim, minutesbeforeobservation = 1000, minutesafterobservation = 1000)
the_output$timeframe
the_output$upcoming_stim
the_output$most_recent_stim
the_output$frequency_observations
the_output$frequency_stimuli
}
\keyword{ stimuli_location }
