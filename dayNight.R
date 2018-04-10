#dayNight

rm(list=ls())
# this one is for getting sun rise and sunset time
library(StreamMetabolism)

# finds day/night of each data point
# datPre <- read.table("RawData/Pre_Tags_Filtered_HRAnalysis.csv",
#                   sep = ",",
#                   header = TRUE)
#
# datPre$Date <- as.POSIXct(strptime(paste(datPre$TransmissionDate, datPre$Time_),
#                                    format = "%m/%d/%Y %H:%M:%S"))
#
# datPre1 <- subset(datPre, select = c("ArgosID", "Date", "Lat", "Lon"))

# post closure has different format... :(
datPost <- read.table("RawData/Post_Tags_Filtered_HRAnalysis_datetimecorrect.csv",
                      sep = ",",
                      header = TRUE)

datPost$Date <- as.POSIXct(strptime(datPost$TransmissionDateTime,
                                    format = "%m/%d/%Y %H:%M"))
datPost1 <- subset(datPost, select = c("ArgosID", "Date", "Lat", "Lon"))

# combine them together.
#dat <- na.omit(rbind(datPre1, datPost1))
dat <- na.omit(datPost1)

rise <- set <- vector(mode = "numeric", length = dim(dat)[1])

#dim(dat)[1]
for (k in 1:dim(dat)[1]){
  tmp <- sunrise.set(dat[k,"Lat"], dat[k, "Lon"],
                     dat[k, "Date"],
                     timezone = "America/Los_Angeles")

  rise[k] <- tmp$sunrise
  set[k] <- tmp$sunset
}

dat$sunrise <- as.POSIXct(x=rise, origin = "1970-01-01 00:00.00 UTC")
dat$sunset <- as.POSIXct(x=set, origin = "1970-01-01 00:00.00 UTC")

dat$daynight <- ifelse((dat$Date>dat$sunrise & dat$Date < dat$sunset),
                       1, 2)

uniqIDs <- unique(dat$ArgosID)

for (k in 1:length(uniqIDs)){
  outfile <- paste0(uniqIDs[k], 'DayNight_14Apr2016.csv')
  datTmp <- dat[dat$ArgosID == uniqIDs[k],]
  write.csv(datTmp, file = outfile, quote = F, row.names=F)
}





