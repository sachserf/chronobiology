stimuli_location <- function(df_observed, df_stimuli, minutesbeforeobservation = 0, minutesafterobservation = 0){
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
  # unique locations:
  uni_obs <- as.character(unique(df_observed[,1]))
  uni_stim <- as.character(unique(df_stimuli[,1]))

  # BOTH DATASETS
  # coincident locations
  value <- ifelse(uni_obs %in% uni_stim == TRUE, uni_obs, NA)
  # remove NAs
  if(any(is.na(value)) == TRUE) {
    value <- value[-which(is.na(value) == TRUE)]
  }
  # reduce data due to coincident locations
  df_observed <- df_observed[which(df_observed[,1] %in% value),]
  df_stimuli <- df_stimuli[which(df_stimuli[,1] %in% value),]
  # all possible combinations of data per location:
  df_all <- merge(x = df_observed,
                  y = df_stimuli,
                  by = 1)

  ### TIMEFRAME
  # time difference between observation and stimulus [minutes]
  # negative value: stimulus before the observation
  # positive value: stimulus after the observation
  df_all$diff_minutes <- difftime(df_all[,c(ncolobs + 1)],
                                  df_all[,2],
                                  units = "mins")


  # use min and max, if input-timeframe is 0
  if(minutesbeforeobservation == 0 & minutesafterobservation == 0) {
    minutesbeforeobservation <- as.numeric(min(df_all$diff_minutes))*-1
    minutesafterobservation <- as.numeric(max(df_all$diff_minutes))
  }

  # reduce data to timeframe
  df_maxtime <-
    df_all[which(df_all$diff_minutes <=
                       minutesafterobservation &
                   df_all$diff_minutes >=
                       -minutesbeforeobservation)
               , ]
  #reduce data to the upcoming stimulus
  # ave: min time-diff per row_ID (Observation)
  df_upcoming_stim <-
    suppressWarnings(df_all[df_all$diff_minutes >= 0 &
                              df_all$diff_minutes ==
                                  ave(df_all$diff_minutes,
                                      df_all$O_rowID,
                                      FUN = min)
                                , ])
  # reduce data to most recent stimulus
  # ave: max time-diff per row_ID (Observation)
  df_most_recent_stim <-
    suppressWarnings(df_all[df_all$diff_minutes < 0 &
                              df_all$diff_minutes ==
                                  ave(df_all$diff_minutes,
                                      df_all$O_rowID,
                                      FUN = max)
                                , ])

  ### Frequency of data
  data.frame(table(df_obs_in$O_rowID))
  data.frame(table(df_maxtime$O_rowID))
  table(df_maxtime$O_rowID)
  df_maxtime$O_rowID

  if(nrow(df_maxtime) != 0){
    df_obs_in$timeframe <-
      suppressWarnings(merge(data.frame(table(df_obs_in$O_rowID)),
                             data.frame(table(df_maxtime$O_rowID)),
                             by = "Var1",
                             all.x = TRUE)[3])
    names(df_obs_in$timeframe) <- "timeframe"

    df_stim_in$timeframe <-
      suppressWarnings(merge(data.frame(table(df_stim_in$S_rowID)),
                             data.frame(table(df_maxtime$S_rowID)),
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
  if(nrow(df_most_recent_stim) != 0){
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
  list_out <- list(timeframe = df_maxtime,
                   upcoming_stim = df_upcoming_stim,
                   most_recent_stim = df_most_recent_stim,
                   frequency_observations = df_obs_in,
                   frequency_stimuli = df_stim_in)
  return(list_out)
}
