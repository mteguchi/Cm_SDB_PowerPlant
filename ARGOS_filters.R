
# filters ARGOS locations for certain LC values, then figure out
# day/night based on the local sunrise/sunset. Results are
# written into files. Use splitinto4hrs script to thin data points


rm(list = ls())
library(dplyr)
library(data.table)  # for combining data.frames.
# this one is for getting sun rise and sunset time

library(StreamMetabolism)

# these are raw data files.

LCs<-c("1","2","3")

all.files <- as.list(list.files("data/", pattern = "_16Jun2016.csv"))

all.data <- lapply(all.files,
                   FUN = function(x) read.csv(paste0("data/", x)))

#filters each Argos tag for nums (LC 1,2,3) and GPS, which eliminates Argos A,B,Z,0
## | represents or

filtered.data <- lapply(all.data,
                        FUN = function(x){
                          filter(x, LC == LCs[1] |
                                   LC == LCs[2] |
                                   LC == LCs[3] |
                                   Message_Type=="GPS")})

#Create a vector that binds all pre tag data into one dataframe
data.all <- rbindlist(filtered.data)
data.all$ArgosID <- as.factor(data.all$ArgosID)

#dayNight, classify each relocation as day or night
# finds day/night of each data point for pre closure

#""  "0" "1" "2" "3" "A" "B" "Z"
##this next step gets rid of those hidden levels and puts in NA
##if you dont want that then this isnt the right way to do this
levels(data.all$LC)[levels(data.all$LC) %in% c("","A","B","Z")]<-NA


##convert all variables of interest to same type (character)
data.all$Message_Type<-as.character(data.all$Message_Type)
data.all$Residual<-as.character(data.all$Residual)
data.all$LC<-as.character(data.all$LC)

##now replace NA's with Message_Type
data.all$Residual[is.na(data.all$Residual)]<-data.all$Message_Type[is.na(data.all$Residual)]
data.all$LC[is.na(data.all$LC)]<-data.all$Message_Type[is.na(data.all$LC)]

dat.all1 <- subset(data.all,
                   select = c("ArgosID",
                              "Message_Type",
                              "TransmissionDateTime",
                              "LC",
                              "Residual",
                              "Lat1", "Lon1"))

#View(datPre1), datPre1 subset looks good

dat.all1$Date <- as.POSIXct(strptime(dat.all1$TransmissionDateTime,
                                     "%m/%d/%y %H:%M"))
dat.all1$Date2 <- as.character(dat.all1$Date, format = "%Y/%m/%d")

dat <- na.omit(dat.all1)

#View(dat), looks good

rise <- set <- vector(mode = "numeric", length = dim(dat)[1])

#dim(dat)[1]
for (k in 1:dim(dat)[1]){
  tmp <- sunrise.set(lat = dat$Lat1[k],
                     long = dat$Lon1[k],
                     date = dat$Date[k],
                     timezone = "America/Los_Angeles")

  rise[k] <- tmp$sunrise
  set[k] <- tmp$sunset
}

dat$sunrise <- as.POSIXct(x=rise, origin = "1970-01-01 00:00.00 UTC")
dat$sunset <- as.POSIXct(x=set, origin = "1970-01-01 00:00.00 UTC")

dat$daynight <- ifelse((dat$Date>dat$sunrise & dat$Date < dat$sunset),
                       1, 2)

uniqIDs <- unique(dat$ArgosID)
#View(uniqIDs)

for (k in 1:length(uniqIDs)){
  outfile <- paste0("data/", uniqIDs[k], '_DayNight',
                    format(Sys.Date(), "_%Y-%m-%d"), '.csv')
  datTmp <- dat[dat$ArgosID == uniqIDs[k],]
  write.csv(datTmp, file = outfile, quote = F, row.names=F)
}

# figure out how to automate inside/outside determination
