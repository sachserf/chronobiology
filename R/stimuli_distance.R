stimuli_distance <- function(df_observed, df_stimuli, minutesbeforeobservation = 0, minutesafterobservation = 0, maxdist = FALSE, distance_algorithm = geosphere::distVincentyEllipsoid){
  # OBSERVATIONS
  # definitions
  df_observed$rowID <- 1:nrow(df_observed)
  names(df_observed) <- paste("O_",
                              names(df_observed),
                              sep = "")
  df_obs_in <- df_observed  # backup
  ncolobs <- ncol(df_observed)
  # STIMULI
  # definitions
  df_stimuli$rowID <- 1:nrow(df_stimuli)
  names(df_stimuli) <- paste("S_",
                             names(df_stimuli),
                             sep = "")
  df_stim_in <- df_stimuli  # backup
  ###### compute distance
  # output in [m]
  # rows: X (observations)
  # cols: Y (stimulations)
  distmatrix <- geosphere::distm(x = df_observed[,1:2],
                                 y = df_stimuli[,1:2],
                                 fun = distance_algorithm)

  ### ALL POSSIBLE COMBINATIONS
  df_allcombinations <- merge(x = df_observed, y = df_stimuli)

  ### SPATIAL EXTENT
  diff_meters <- c(distmatrix) # all "variables" (stimuli) in one col
  df_all <- cbind(df_allcombinations, diff_meters)

  ### TIMEFRAME
  # time difference between observation and stimulus [minutes]
  # negative value: stimulus before the observation
  # positive value: stimulus after the observation
  df_all$diff_minutes <- difftime(df_all[,c(ncolobs + 3)],
                                  df_all[,3],
                                  units = "mins")

  # use maximum distance, if input is FALSE
  if(maxdist == FALSE) {
    maxdist <- max(df_all$diff_meters)
  }
  # reduce data to spatial extent
  df_maxdist <- df_all[df_all$diff_meters <= maxdist,]

  # use min and max, if input-timeframe is 0
  if(minutesbeforeobservation == 0 & minutesafterobservation == 0) {
    minutesbeforeobservation <- as.numeric(min(df_maxdist$diff_minutes))*-1
    minutesafterobservation <- as.numeric(max(df_maxdist$diff_minutes))
  }

  # reduce data to timeframe
  df_maxdisttime <-
    df_maxdist[which(df_maxdist$diff_minutes <=
                       minutesafterobservation &
                       df_maxdist$diff_minutes >=
                       -minutesbeforeobservation)
               , ]
  #reduce data to the upcoming stimulus
  # ave: min time-diff per row_ID (Observation)
  df_upcoming_stim <-
    suppressWarnings(df_maxdist[df_maxdist$diff_minutes >= 0 &
                                  df_maxdist$diff_minutes ==
                                  ave(df_maxdist$diff_minutes,
                                      df_maxdist$O_rowID,
                                      FUN = min)
                                , ])
  # reduce data to most recent stimulus
  # ave: max time-diff per row_ID (Observation)
  df_most_recent_stim <-
    suppressWarnings(df_maxdist[df_maxdist$diff_minutes < 0 &
                                  df_maxdist$diff_minutes ==
                                  ave(df_maxdist$diff_minutes,
                                      df_maxdist$O_rowID,
                                      FUN = max)
                                , ])

  ### Frequency of data
  data.frame(table(df_obs_in$O_rowID))
  data.frame(table(df_maxdisttime$O_rowID))
  table(df_maxdisttime$O_rowID)
  df_maxdisttime$O_rowID

  if(nrow(df_maxdisttime) != 0){
    df_obs_in$timeframe <-
      suppressWarnings(merge(data.frame(table(df_obs_in$O_rowID)),
                             data.frame(table(df_maxdisttime$O_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_obs_in$timeframe) <- "timeframe"

    df_stim_in$timeframe <-
      suppressWarnings(merge(data.frame(table(df_stim_in$S_rowID)),
                             data.frame(table(df_maxdisttime$S_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_stim_in$timeframe) <- "timeframe"
  }
  if(nrow(df_upcoming_stim) != 0){
    df_obs_in$df_upcoming_stim <-
      suppressWarnings(merge(data.frame(table(df_obs_in$O_rowID)),
                             data.frame(table(df_upcoming_stim$O_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_obs_in$df_upcoming_stim) <- "upcoming_stim"
    df_stim_in$df_upcoming_stim <-
      suppressWarnings(merge(data.frame(table(df_stim_in$S_rowID)),
                             data.frame(table(df_upcoming_stim$S_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_stim_in$df_upcoming_stim) <- "upcoming_stim"
  }
  if(nrow(df_upcoming_stim) != 0){
    df_obs_in$df_most_recent_stim <-
      suppressWarnings(merge(data.frame(table(df_obs_in$O_rowID)),
                             data.frame(table(df_most_recent_stim$O_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_obs_in$df_most_recent_stim) <- "most_recent_stim"
    df_stim_in$df_most_recent_stim <-
      suppressWarnings(merge(data.frame(table(df_stim_in$S_rowID)),
                             data.frame(table(df_most_recent_stim$S_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_stim_in$df_most_recent_stim) <- "most_recent_stim"
  }
  ### RETURN OUTPUT
  list_out <- list(timeframe = df_maxdisttime,
                   upcoming_stim = df_upcoming_stim,
                   most_recent_stim = df_most_recent_stim,
                   frequency_observations = df_obs_in,
                   frequency_stimuli = df_stim_in)
  return(list_out)
}
