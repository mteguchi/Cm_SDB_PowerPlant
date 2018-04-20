
# filters ARGOS locations for certain LC values, then figure out
# day/night based on the local sunrise/sunset. Results are
# written into files. Use splitinto4hrs script to thin data points

#filters each Argos tag for nums (LC 1,2,3) and GPS, which eliminates Argos A,B,Z,0
## | represents or

ARGOS_filter.1 <- function(all.data){
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
                                                              format = "%m-%d-%Y %H:%M:%S",
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

  return(filtered.rise.set)
}

ARGOS_filter.2 <- function(all.data){
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

  return(filtered.rise.set)
}

# figure out how to automate inside/outside determination
