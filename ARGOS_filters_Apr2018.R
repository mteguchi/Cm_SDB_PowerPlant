
# filters ARGOS locations for certain LC values, then figure out
# day/night based on the local sunrise/sunset. Results are
# written into files. Use splitinto4hrs script to thin data points


rm(list = ls())
library(dplyr)
library(lubridate)
#library(data.table)  # for combining data.frames.
# this one is for getting sun rise and sunset time

library(StreamMetabolism)

# these are raw data files.
# files in the following directory have been moved to data/files_Oct2016 on 04-03-2018
#dName <- "D:/TomosFolders/turtles/greens/SDBay/SDBayTelemetry/files_Oct2016/"
#dName <- "data/files_Oct2016/"
LCs<-c("1","2","3")

all.files <- as.list(list.files("data/", pattern = "_inside.csv"))

ID.names <- function(name){
  x <- unlist(strsplit(name, '_'))[1]
  return(x)
}

IDs <- unlist(lapply(all.files, FUN = ID.names))

all.data <- lapply(all.files,
                   FUN = function(x) read.csv(paste0("data/", x)))


# get the beginning and end dates of each data file
get.dates <- function(data.file){
  data.file$Date <- as.POSIXct(strptime(data.file$TransmissionDateTime,
                                        "%m/%d/%Y %H:%M:%S"),
                               tz = "GMT")
  #data.file$Date2 <- as.character(data.file$Date, format = "%Y/%m/%d")
  date.begin <- min(data.file$Date, na.rm = T)
  date.end <- max(data.file$Date, na.rm = T)
  out <- list(begin = date.begin, end = date.end)
  return(out)
}

dates.begin.end <- lapply(all.data, FUN = get.dates)

#filters each Argos tag for nums (LC 1,2,3) and GPS, which eliminates Argos A,B,Z,0
## | represents or

filtered.data <- lapply(all.data,
                        FUN = function(x){
                          filter(x, LC == LCs[1] |
                                   LC == LCs[2] |
                                   LC == LCs[3] |
                                   Message_Type=="GPS") %>%
                            select(ID, ArgosID, Message_Type,
                                   TransmissionDateTime,
                                   LC, Residual,
                                   Lat1, Lon1, inside) %>%
                            mutate(UTCDateTime = as.POSIXct(TransmissionDateTime,
                                                            format = "%m/%d/%Y %H:%M:%S",
                                                            tz = "GMT")) %>%
                            mutate(LocalDateTime = with_tz(UTCDateTime,
                                                           tzone = "America/Los_Angeles")) %>%
                            mutate(Date2 = as.character(LocalDateTime,
                                                        format = "%Y/%m/%d")) %>%
                            filter(., !is.na(Lat1))})

# then find sunrise/sunset for each line of the data files:
filtered.rise.set <- lapply(filtered.data,
                            FUN = function(x){
                              rise <- set <- vector(mode = "numeric",
                                                    length = dim(x)[1])

                              for (k in 1:dim(x)[1]){
                                tmp <- sunrise.set(lat = x$Lat1[k],
                                                   long = x$Lon1[k],
                                                   date = x$Date2[k],
                                                   timezone = "America/Los_Angeles")

                                rise[k] <- tmp$sunrise
                                set[k] <- tmp$sunset
                              }

                              x$LocalSunrise <- as.POSIXct(rise,
                                                      origin = "1970-01-01 00:00.00 UTC")
                              x$LocalSunset <- as.POSIXct(set,
                                                     origin = "1970-01-01 00:00.00 UTC")

                              x$day1night2 <- ifelse((x$LocalDateTime > x$LocalSunrise &
                                                      x$LocalDateTime < x$LocalSunset),
                                                   1, 2)
                              return(x)
                            })

for (k in 1:length(filtered.rise.set)){

  outfile <- paste0("data/", IDs[k], '_inside_DayNight_',
                    Sys.Date(), '.csv')

  write.csv(filtered.rise.set[[k]],
            file = outfile,
            quote = F, row.names=F)
}

# figure out how to automate inside/outside determination
