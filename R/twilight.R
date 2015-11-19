twilight <-
function(data_POSIXct,
         latitude,
         longitude,
         twilight = "civil") {
     # specify timezone by attribute of POSIXct
     tzone <- attributes(data_POSIXct)$tzone
     # specify dayofyear (=T)
     dayofyear <- as.numeric(strftime(data_POSIXct,
                                      format="%j",
                                      tz = tzone))
     # difference between timezone of data and UTC
     hours2UTC <- as.numeric(as.character(strftime(data_POSIXct,
                                                   format="%z",
                                                   tz = tzone)))/100
     # compute sunrise and sunset
     # twilight: "rise/set" or "civil" or "nautic")
     # source: http://lexikon.astronomie.info/zeitgleichung/
     truediff = -0.171*sin(0.0337 * dayofyear + 0.465) - 0.1299*sin(0.01787 * dayofyear - 0.168)
     declin = 0.4095*sin(0.016906*(dayofyear-80.086))
     B = pi *latitude / 180
     # process twilight-option
     if (twilight=="rise/set") h=-50/60
     if (twilight=="civil")  h=-6
     if (twilight=="nautic") h=-12
     # calculate sunrise/sunset
     h=h/(360/(2*pi))
     timedev = 12*acos((sin(h) - sin(B)*sin(declin)) / (cos(B)*cos(declin)))/pi
     localrise = 12 - timedev - truediff
     localset = 12 + timedev - truediff
     sunrise_decimal = localrise - longitude /15 + hours2UTC # decimal output
     sunset_decimal = localset -longitude /15 + hours2UTC # decimal output
     # temporary variable for actual POSIXct - date with fixed time:
     tempconv <- strftime(x = data_POSIXct, format = "%Y-%m-%d 00:00:00",
                          tz = tzone, usetz = TRUE)
     tempconv <- as.POSIXct(tempconv, tz = tzone)
     # define sunrise and sunset in POSIXct format
     sunrise <- tempconv + (3600*sunrise_decimal)
     sunset <- tempconv + (3600*sunset_decimal)
     # return dataframe
     return(data.frame(sunrise,sunset))
}

