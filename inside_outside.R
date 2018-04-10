

# determine if locations are in the bay

rm(list=ls())

library(sp)

# use point.in.polygon in sp package

SDBay <- read.csv("data/SDBay_polygon.csv")

data.files <- dir("Data/", "DayNight_4hrs_2016-06-16.csv")

for (k in 1:length(data.files)){
  dat <- read.csv(paste0("Data/", data.files[k]))
  in.out <- vector(mode = "numeric", length = dim(dat)[1])
  for (k1 in 1:length(in.out)){
    in.out[k1] <- point.in.polygon(dat$Lon1[k1], dat$Lat1[k1],
                                   SDBay$long, SDBay$lat)
  }
  dat$inside <- in.out
  write.csv(dat,
            file = paste0("Data/",
                          unlist(strsplit(data.files[k],
                                          '_DayNight_4hrs_'))[1],
                          "_DayNight_4hrs_inout_",
                          unlist(strsplit(data.files[k],
                                          '_DayNight_4hrs_'))[2]),
            quote = F, row.names = F)

  #plot(SDBay$long, SDBay$lat, type = 'l')
  #points(dat$Lon1[in.out == 1], dat$Lat1[in.out == 1], col = 'green')
  #points(dat$Lon1[in.out == 0], dat$Lat1[in.out == 0], col = 'red')
}


