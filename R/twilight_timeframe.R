twilight_timeframe <-
function(data_POSIXct,
         latitude,
         longitude,
         twilight = "civil",
         min2sunrise = 0,
         minaftersunrise = 0,
         min2sunset = 0,
         minaftersunset = 0) {
     # specify input-timezone
     tzone <- attributes(data_POSIXct)$tzone
     # use separate function to calculate photoperiod
     df_photo <- twilight(data_POSIXct = data_POSIXct,
                          latitude = latitude,
                          longitude = longitude,
                          twilight = twilight)
     # compute daylength
     daylength <- difftime(time1 = df_photo$sunset,
                           time2 = df_photo$sunrise,
                           tz = tzone,
                           units = "hours")
     # difference between <<actual time>> and sunrise/sunset in minutes:
     # negative = before, positive = after
     minutes2sunrise <- difftime(time1 = data_POSIXct,
                              time2 = df_photo$sunrise,
                              tz = tzone,
                              units = "mins") # time1 - time2
     minutes2sunset <- difftime(time1 = data_POSIXct,
                             time2 = df_photo$sunset,
                             tz = tzone,
                             units = "mins") # specify output unit
     # formatting
     t2sr <- as.numeric(minutes2sunrise)
     t2ss <- as.numeric(minutes2sunset)
     # compute binary variable: TRUE if <<actual time>> is during the day
     daylight <- ifelse(minutes2sunset > 0 | minutes2sunrise < 0, FALSE, TRUE)
     # create dataframe
     df_sunfun <- data.frame(daylength, daylight, minutes2sunrise, minutes2sunset)
     # compare actual time with timeframe and classify lighting condition
     lightcon <- ifelse(t2sr < -min2sunrise | t2ss > minaftersunset, "night",
                           ifelse(t2sr < 0 & t2sr >= -min2sunrise, "before_sunrise",
                                  ifelse(t2sr > 0 & t2sr <= minaftersunrise, "after_sunrise",
                                         ifelse(t2ss < 0 & t2ss >= -min2sunset, "before_sunset",
                                                ifelse(t2ss > 0 & t2ss <= minaftersunset, "after_sunset",
                                                       "day")))))
     # dusk or dawn
     duskdawn <- as.factor(ifelse(lightcon == "before_sunrise" | lightcon == "after_sunrise", "dawn",
                                  ifelse (lightcon == "before_sunset" | lightcon == "after_sunset", "dusk",
                                          as.character(lightcon))))
     # timeframe binary
     lightcon_bin <- ifelse(lightcon == "night" | lightcon == "day",
                            FALSE, TRUE)
     # create data.frame of timeframe-data
     df_timeframe <- data.frame(lightcon, duskdawn, lightcon_bin)
     # return timeframe only if specified
     timeframe <- min2sunrise + minaftersunrise + min2sunset + minaftersunset
     if(timeframe > 0){
          return(cbind(df_photo, df_sunfun, df_timeframe))
     } else {
          # return final data.frame
          return(cbind(df_photo, df_sunfun))
     }
     }
