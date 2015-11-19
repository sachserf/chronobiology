custom_cycle <- function(data_POSIXct, starting_point = NULL, hours_part_A, hours_part_B = 0){
  if (is.null(starting_point) == TRUE) {
    starting_point <- min(data_POSIXct)
  }
  # formatting
  hours_part_A <- as.difftime(hours_part_A, units = "hours")
  hours_part_B <- as.difftime(hours_part_B, units = "hours")
  tzone <- attributes(start)$tzone
  end <- max(data_POSIXct)
  duration_AB <- hours_part_A+hours_part_B
  duration_SE <- difftime(end, starting_point, tz = tzone, units = "hours")
  nr_cycles <- ceiling(as.numeric(duration_SE)/as.numeric(duration_AB))
  newend <- starting_point + nr_cycles*duration_AB
  # cycle start A
  cycle_start <- seq(starting_point,
                     end,
                     duration_AB)
  begin_A <- as.POSIXct(cycle_start)
  # cycle start B
  cycle_change <- seq(starting_point+as.difftime(hours_part_A, units = "hours"),
                      newend,
                      duration_AB)
  begin_B <- as.POSIXct(cycle_change)
  # starting point upcoming cycle
  cycle_new <- seq(starting_point+duration_AB,
                   newend,
                   duration_AB)
  uc_cycle <- as.POSIXct(cycle_new) # uc_cycle = upcoming cycle
  # combine cycle starting points
  df_cycle <- data.frame(A = begin_A, B = begin_B, next_cycle = uc_cycle)
  df_cycle$cycle_NR <- as.integer(row.names(df_cycle))
  # compare cycle with observations (data_POSIXct)
  cycle_NR_fun <- function(x){
    suppressWarnings(min(which(df_cycle$next_cycle > data_POSIXct[x])))
  }
  cycle_NR <- sapply(X = 1:length(data_POSIXct), FUN = cycle_NR_fun)
  temp_df_orig <- data.frame(data_POSIXct, cycle_NR)
  # combine information of cycles with original data by cycle_NR
  df_final <- merge(x = temp_df_orig, y = df_cycle, by = "cycle_NR")
  # compare observed data vs part of cycle
  df_final$cycle_ID <- ifelse(df_final$data_POSIXct >= df_final$A & df_final$data_POSIXct < df_final$B, "A",
                              ifelse(df_final$data_POSIXct >= df_final$B & df_final$data_POSIXct < df_final$next_cycle, "B",
                                     FALSE))
  df_final$cycle_NR[which(df_final$cycle_ID == FALSE)] <- NA
  return(df_final)
}
