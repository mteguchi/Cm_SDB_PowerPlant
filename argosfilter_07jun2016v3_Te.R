rm(list = ls())  #clears all vectors in environment
library(dplyr)
library(data.table)  # for combining data.frames.

# make sure that this the wd setwd("/sdb/gst/hr_analysis2016")

#getwd()
#list.files()
#ls()

#load pre data and create vectors for each pre tag

dnamePre <- "argosdata/pre_1/pre/"
dnamePost <- "argosdata/post_1/post/"
LCs<-c("1","2","3")


all.files.pre <- as.list(list.files(dnamePre, pattern = ".csv"))

all.data.pre <- lapply(all.files.pre,
                       FUN = function(x) read.csv(paste0(dnamePre, x)))

#filters each Argos tag for nums (LC 1,2,3) and GPS, which eliminates Argos A,B,Z,0
## | represents or

filtered.data.pre <- lapply(all.data.pre,
                            FUN = function(x){
                              filter(x, LC == LCs[1] |
                                       LC == LCs[2] |
                                       LC == LCs[3] |
                                       Message_Type=="GPS")})

#Create a vector that binds all pre tag data into one dataframe
Pre.all <- rbindlist(filtered.data.pre)
Pre.all$ArgosID <- as.factor(Pre.all$ArgosID)

# View(Pre.all)

all.files.post <- as.list(list.files(dnamePost, pattern = ".csv"))
all.data.post <- lapply(all.files.post,
                        FUN = function(x) read.csv(paste0(dnamePost, x)))

#Filter out Argos A,B,Z,0 for post tag data
## can't do LC == LCs because you can't compare multiple lines with
## multiple possibilities.
filtered.data.post <- lapply(all.data.post,
                            FUN = function(x){
                              filter(x, LC == LCs[1] |
                                       LC == LCs[2] |
                                       LC == LCs[3] |
                                       Message_Type=="GPS")})


Post.all <- rbindlist(filtered.data.post)
Post.all$ArgosID <- as.factor(Post.all$ArgosID)

#dayNight, classify each relocation as day or night

# this one is for getting sun rise and sunset time

library(StreamMetabolism)

# finds day/night of each data point for pre closure

datPre <- Pre.all
datPost <- Post.all

#View(datPre)
#View(datPost)

#need to add code for column merging
#want to merge columns so that LC and residual can be added in as a column for GIS metadata
#if LC is blank, want to merge with data found in Message_Type
#if Residual is blank, want to merge with data found in Message_Type

##I am guessing the issue you had is because oyu are trying to mix
##vector types (numeric and factor) into a string which R does not like
##the easiest solution I have is to call them all characters and then do the replacement
##LC also had blanks instead of NA's so first I convert those to NA's
##this can also be done by including 'na.strings = "NA"' after 'write.csv' calls

#looking at what kind type of variables the columns are
#str(datPre)
#str(datPost)
#str(datPre$LC) #this one seems to have hidden levels
##even though LC should be numeric it is registering as a factor
##what are they?
levels(datPre$LC)
levels(datPost$LC)
#""  "0" "1" "2" "3" "A" "B" "Z"
##this next step gets rid of those hidden levels and puts in NA
##if you dont want that then this isnt the right way to do this
levels(datPre$LC)[levels(datPre$LC) %in% c("","A","B","Z")]<-NA
levels(datPost$LC)[levels(datPost$LC) %in% c("","A","B","Z")]<-NA

##convert all variables of interest to same type (character)
datPre$Message_Type<-as.character(datPre$Message_Type)
datPre$Residual<-as.character(datPre$Residual)
datPre$LC<-as.character(datPre$LC)

datPost$Message_Type<-as.character(datPost$Message_Type)
datPost$Residual<-as.character(datPost$Residual)
datPost$LC<-as.character(datPost$LC)

##now replace NA's with Message_Type
datPre$Residual[is.na(datPre$Residual)]<-datPre$Message_Type[is.na(datPre$Residual)]
datPre$LC[is.na(datPre$LC)]<-datPre$Message_Type[is.na(datPre$LC)]

datPost$Residual[is.na(datPost$Residual)]<-datPost$Message_Type[is.na(datPost$Residual)]
datPost$LC[is.na(datPost$LC)]<-datPost$Message_Type[is.na(datPost$LC)]

#View(datPre)
#View(datPost)

#columns look correct at this point

# combine them together

#code below runs but fails for sunrise_set calculations because of the blanks in LC and Residual

datPre1 <- subset(datPre,
                  select = c("ArgosID",
                             "Message_Type",
                             "TransmissionDateTime",
                             "LC",
                             "Residual",
                             "Lat1", "Lon1"))

#View(datPre1), datPre1 subset looks good

datPre1$Date <- as.POSIXct(strptime(datPre1$TransmissionDateTime,
                                    "%m-%d-%Y %H:%M:%S"))
datPre1$Date2 <- as.character(datPre1$Date, format = "%Y/%m/%d")

# finds day/night of each data point for post closure

datPost1 <- subset(datPost, select = c("ArgosID",
                                       "Message_Type",
                                       "TransmissionDateTime",
                                       "LC",
                                       "Residual",
                                       "Lat1", "Lon1"))

#View(datPost1), datpost1 subset looks good

datPost1$Date <- as.POSIXct(strptime(datPost1$TransmissionDateTime,
                                     "%m-%d-%Y %H:%M:%S"))

# Just to show how we can convert dates back into a different format
datPost1$Date2 <- as.character(datPost1$Date, format = "%Y/%m/%d")

dat <- na.omit(rbind(datPre1, datPost1))

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
  outfile <- paste0(uniqIDs[k], '_DayNight',
                    format(Sys.Date(), "_%Y-%m-%d"), '.csv')
  datTmp <- dat[dat$ArgosID == uniqIDs[k],]
  write.csv(datTmp, file = outfile, quote = F, row.names=F)
}

# figure out how to automate inside/outside determination
