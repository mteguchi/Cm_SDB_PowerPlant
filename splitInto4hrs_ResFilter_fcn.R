

get4hrs <- function(x, res.limit = 35.0){
  #fileout <- paste0(data, '/', id[1], "_DayNight_4hrs",
  #                  format(Sys.Date(), "_%Y-%m-%d"), ".csv")
  begin.date <- as.Date(min(as.POSIXct(strptime(x$LocalDateTime,
                                                "%Y-%m-%d %H:%M:%S"),
                                       tz = "America/Los_Angeles")),
                        tz = "America/Los_Angeles")
  end.date <- as.Date(max(as.POSIXct(strptime(x$LocalDateTime,
                                              "%Y-%m-%d %H:%M:%S"),
                                     tz = "America/Los_Angeles")),
                      tz = "America/Los_Angeles")

  x$row <- seq(from = 1, to = dim(x)[1])
  x$include <- 0
  x$hr <- hour(x$LocalDateTime)
  hrs <- c(0, 4, 8, 12, 16, 20)
  for (k in begin.date:end.date){
    tmp1 <- filter(x, as.Date(LocalDateTime, tz = "America/Los_Angeles") == k)
    for (k1 in 1:length(hrs)){
      tmp2 <- tmp1[tmp1$hr >= hrs[k1] &
                     tmp1$hr < (hrs[k1]+4), ]

      # for every 4 hour block:
      if (dim(tmp2)[1] != 0){
        # first look for GPS data
        if (length(grep("GPS", tmp2$Message_Type)) > 0){
          tmp2.1 <- filter(tmp2, Message_Type == "GPS") %>%
            filter(Residual == min(Residual)) %>%
            filter(Residual <= res.limit)
          # if no good gps data, i.e. residual > res.llimit.
          if (nrow(tmp2.1) == 0){
            tmp2.1 <- filter(tmp2, Message_Type != "GPS")
            # and if there is at least one Argos data
            if (nrow(tmp2.1) > 0){
              tmp2.1 <- tmp2.1[as.numeric(tmp2.1$LC) ==
                                 max(as.numeric(tmp2.1$LC)),]
            }
          }

        } else {  # if there is no GPS data:
          tmp2.1 <- tmp2[tmp2$LC == max(tmp2$LC),]
        }

        if (nrow(tmp2.1) > 0) x[tmp2.1$row[1], 'include'] <- 1
      }
    }
  }

  #x <- filter(x, include == 1)
  if (dim(x)[1] > 0){
    return(x)
  } else {
    return(NA)
  }
}
