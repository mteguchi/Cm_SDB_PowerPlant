

# determine if locations are in the bay

rm(list=ls())

library(sp)

# files in the following directory have been moved to data/files_Oct2016 on 04-03-2018
#dName <- "D:/TomosFolders/turtles/greens/SDBay/SDBayTelemetry/files_Oct2016/"

# use point.in.polygon in sp package

dName <- "data/files_Oct2016/"
SDBay <- read.csv("data/SDBay_polygon.csv")

data.files <- dir(dName, pattern = ".txt")

for (k in 1:length(data.files)){
  dat <- read.csv(paste0("data/", data.files[k]))
  in.out <- vector(mode = "numeric", length = dim(dat)[1])
  for (k1 in 1:length(in.out)){
    in.out[k1] <- point.in.polygon(dat$Lon1[k1], dat$Lat1[k1],
                                   SDBay$long, SDBay$lat)
  }
  dat$inside <- in.out

  write.csv(dat,
            file = paste0("data/", unlist(strsplit(data.files[k], '_'))[1],
                          "_inout.csv"),
            quote = F, row.names = F)

  dat.inside <- filter(dat, inside == 1)
  write.csv(dat.inside,
            file = paste0("data/", unlist(strsplit(data.files[k], '_'))[1],
                          "_inside.csv"),
            quote = F, row.names = F)

  #plot(SDBay$long, SDBay$lat, type = 'l')
  #points(dat$Lon1[in.out == 1], dat$Lat1[in.out == 1], col = 'green')
  #points(dat$Lon1[in.out == 0], dat$Lat1[in.out == 0], col = 'red')
}


