circadian_timeframe <-
function (data_POSIXct, begin = "00:00", end = "00:00")
{
  if(begin != end){
    # get timezone
    tzone <- attributes(data_POSIXct)$tzone
    # format custom timeframe
    t1 <- paste("%Y-%m-%d", begin) # begin custum timeframe
    t2 <- paste("%Y-%m-%d", end) # end custom timeframe
    # combine timeframe and original date
    n1 <- as.POSIXct(strftime(x = data_POSIXct,
                              format = t1,
                              tz = tzone),
                     tz = tzone)
    n2 <- as.POSIXct(strftime(x = data_POSIXct,
                              format = t2,
                              tz = tzone),
                     tz = tzone)

      dtf <- data.frame(ifelse(data_POSIXct <= n2 & data_POSIXct >= n1,
                               "inside", "outside"))
      names(dtf) <- paste("T", begin, "_", "T", end, sep = "")
      return(data.frame(dtf))
  } else {
    warning("specify timeframe")
  }
}
