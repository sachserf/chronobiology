\name{stimuli}
\alias{stimuli}
\title{
combine data from observations and stimuli (by location)
}
\description{
The function selects the most recent stimulus (e.g. disturbance event) prior to observation data and combines the information. The function considers different locations. Because of this the input data should be preformatted (see 'Arguments').
}
\usage{
stimuli(df_observed, df_stimuli)
}
\arguments{
  \item{df_observed}{
A data.frame object of observation-data. The first column needs to contain an ID about the location. The second column needs to contain POSIXct-data. Additional columns are optional.
}
  \item{df_stimuli}{
A data.frame object of stimuli-data. The first column needs to contain an ID about the location. The second column needs to contain POSIXct-data. Additional columns are optional.
}
}
\details{
The location column is not optional. If theres only one location in your data simply use a repeated value.
}
\value{
The output is a list object containing two data frames.
\item{observations }{The input data frame 'df_observed' with additional columns due to the most recent stimulus for each observation (per location). Furthermore the Variable 'minafterstim' denotes the 'minutes after stimulation'. It is possible to use identical column names among the input data frames. Therefore the output contains modified column names starting with 'O_' or 'S_' indicating whether the information comes from 'df_observed' or 'df_stimuli'.}
\item{stimuli }{The input data frame 'df_stimuli' with additional columns due to the upcoming observation (per location). For more information see above.}
}
\author{
Frederik Sachser
}
\examples{
now <- as.POSIXct(x = "2015-01-30 20:58:00", tz = "CET", origin = "01/01/1970")
date_obs <- seq(now, now + 90*60*99, 90*60)
date_stim <- sample(date_obs, 15) + 60
loc_stim <- sample(c("A", "B", "C"), length(date_stim), replace = TRUE)
loc_obs <- sample(c("A", "B", "D"), length(date_obs), replace = TRUE)
df_obs <- data.frame(loc_obs, date_obs)
df_stim <- data.frame(loc_stim, date_stim)
the_output <- stimuli(df_observed = df_obs, df_stimuli = df_stim)
the_output$observations
the_output$stimuli
}
\keyword{ stimuli }
