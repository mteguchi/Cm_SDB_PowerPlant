

rm(list=ls())
library(dplyr)
library(lubridate)
#dname <- "D:/TomosFolders/turtles/greens/SDBay/SDBayTelemetry/files_Oct2016/"
all.files <- dir(path = "data/",
                 pattern = "_inside_DayNight_2018-04-04.csv")

all.data <- lapply(all.files,
                   FUN = function(x) read.csv(paste0("data/", x)))

ID.names <- function(name){
  x <- unlist(strsplit(name, '_'))[1]
  return(x)
}

IDs <- unlist(lapply(all.files, FUN = ID.names))

# max residual value
res.limit <- 35.0

# get the beginning and end dates of each data file
get.dates <- function(x){
  Date <- as.POSIXct(strptime(x$LocalDateTime,
                              "%Y-%m-%d %H:%M:%S"),
                     tz = "America/Los_Angeles")
  date.begin <- min(Date, na.rm = T)
  date.end <- max(Date, na.rm = T)
  out <- list(begin = date.begin, end = date.end)
  return(out)
}

dates.begin.end <- lapply(all.data, FUN = get.dates)
options(warn = 2)  # returns warnings as errors and stops executions

get4hrs <- function(x){
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
  for (k in begin.date:end.date){
    tmp1 <- filter(x, as.Date(LocalDateTime, tz = "America/Los_Angeles") == k)
    for (k1 in 1:6){
      tmp2 <- tmp1[tmp1$hr >= ((k1-1) * 4) &
                     tmp1$hr < (k1*4), ]

      # for every 4 hour block:
      if (dim(tmp2)[1] != 0){
        # first look for GPS data
        if (length(grep("GPS", tmp2$Message_Type)) > 0){
          tmp2.1 <- filter(tmp2, Message_Type == "GPS") %>%
            filter(Residual == min(Residual)) %>%
            filter(Residual <= res.limit)
          # if no good gps data, i.e. residual >= res.llimit.
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
        if (nrow(tmp2.1) > 0)

          x[tmp2.1$row[1], 'include'] <- 1
      }
    }
  }
  x <- filter(x, include == 1)
  if (dim(x)[1] > 0){
    return(x)
  } else {
    return(NA)
  }
}

filtered.4hrs <- lapply(all.data,
                        FUN = get4hrs)

for (k in 1:length(filtered.4hrs)){

  outfile <- paste0("data/", IDs[k], '_inside_DayNight_4hrs_',
                    Sys.Date(), '.csv')

  write.csv(filtered.4hrs[[k]],
            file = outfile,
            quote = F, row.names=F)
}







