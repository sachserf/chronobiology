POSIXct_info <-
function(data_POSIXct){
     tzone <- attributes(data_POSIXct)$tzone # specify input-timezone
     POSIX_lt <- as.POSIXlt(data_POSIXct,
                            tz = tzone)
     date <- as.Date(strftime(data_POSIXct,
                              format="%F",
                              tz = tzone)) # ISO 8601 date format
     year <- strftime(data_POSIXct,
                      format="%Y",
                      tz = tzone)
     quarter <- quarters(data_POSIXct)
     months <- months(data_POSIXct)
     dayofyear <- as.numeric(strftime(data_POSIXct,
                                      format="%j",
                                      tz = tzone))
     weekday_short <- strftime(data_POSIXct,
                               format="%a",
                               tz = tzone)
     weekend <- ifelse(strftime(data_POSIXct,
                                format = "%u",
                                tz = tzone) == "6" | strftime(data_POSIXct,
                                                              format = "%u",
                                                              tz = tzone) == "7",
                       TRUE, FALSE)
     julian_date <- julian(data_POSIXct)
     hour <- as.numeric(strftime(data_POSIXct,
                                 format="%H",
                                 tz = tzone))
     partofday <- ifelse(hour >= 0 & hour < 6, "night (00-06)",
                         ifelse(hour >= 6 & hour < 12, "morning (06-12)",
                                ifelse(hour >= 12 & hour < 18, "afternoon (12-18)",
                                       ifelse(hour >= 18 & hour < 24, "evening (18-24)",
                                              NA))))
     time <- strftime(data_POSIXct,
                      format="%X",
                      tz = tzone)
     time_num <- as.numeric(strftime(data_POSIXct,
                                     format="%H.%M",
                                     tz = tzone))
     hours2UTC <- as.numeric(as.character(strftime(data_POSIXct,
                                                   format="%z",
                                                   tz = tzone)))/100
     timezone <- strftime(data_POSIXct,
                          format="%Z",
                          tz = tzone)
     return(data.frame(POSIX_lt, date, year, quarter, months, weekday_short,
                       weekend, julian_date, dayofyear, hour,
                       partofday, time, time_num, hours2UTC, timezone))
}
