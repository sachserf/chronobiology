stimuli <- function(df_observed, df_stimuli){
  # OBSERVATIONS
  # definitions
  df_observed$thisisunique <- 1:nrow(df_observed)
  names(df_observed) <- paste("O_", names(df_observed), sep = "")
  df_obs_in <- df_observed  # backup
  ncolobs <- ncol(df_observed)
  # unique locations:
  uni_obs <- as.character(unique(df_observed[,1]))
  # STIMULI
  # definitions
  df_stimuli$thisisunique <- 1:nrow(df_stimuli)
  names(df_stimuli) <- paste("S_", names(df_stimuli), sep = "")
  df_stim_in <- df_stimuli  # backup
  # unique locations:
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
  # minutes after stimulus
  df_all$minafterstim <- difftime(df_all[,2],
                                  df_all[,c(1 + ncolobs)],
                                  units = "mins")
  # delete negative rows (stimulus later than observation)
  df_all <- df_all[which(df_all$minafterstim > 0),]
  # reduce data to the most recent stimulus for each observation
  df_obs_final <- suppressWarnings(df_all[df_all$minafterstim == ave(df_all$minafterstim,
                                                                     df_all[,1],
                                                                     df_all[,2],
                                                                     FUN = min),])
  # reduce data to the following observation for each stimulus
  df_stim_final <- suppressWarnings(df_all[df_all$minafterstim == ave(df_all$minafterstim,
                                                                      df_all[,(ncolobs + 1)],
                                                                      df_all[,(ncolobs + 2)],
                                                                      FUN = min),])
  # combine original observation data to keep all cases:
  df_obs_out <- merge(x = df_obs_in,
                      y = df_obs_final,
                      by.x = "O_thisisunique",
                      by.y = "O_thisisunique",
                      all.x = TRUE)
  df_stim_out <- merge(x = df_stim_in,
                       y = df_stim_final,
                       by.x = "S_thisisunique",
                       by.y = "S_thisisunique",
                       all.x = TRUE)
  # rm thisisunique and doubled variables:
  df_obs_out <- df_obs_out[, -c(1, (ncolobs+1):(2*ncolobs-1), (ncol(df_obs_out)-1))]
  df_stim_out <- df_stim_out[, -c(1, (ncol(df_stim_in) + ncolobs):(ncol(df_stim_out)-1))]
  # rm xy in names
  names(df_obs_out) <- ifelse(names(df_obs_out) %in% c(names(df_obs_in),
                                                       names(df_stim_in),
                                                       'minafterstim'),
                              names(df_obs_out),
                              strtrim(names(df_obs_out), nchar(names(df_obs_out))-2))
  names(df_stim_out) <- ifelse(names(df_stim_out) %in% c(names(df_obs_in),
                                                         names(df_stim_in),
                                                         'minafterstim'),
                               names(df_stim_out),
                               strtrim(names(df_stim_out), nchar(names(df_stim_out))-2))
  # return output:
  list_out <- list(observations = df_obs_out,
                   stimuli = df_stim_out)
  return(list_out)
}
