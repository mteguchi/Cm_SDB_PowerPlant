#plot152322


# plots path of 152322, which went to Revillagigedos
rm(list=ls())
library(maps)          # creating geographical maps
library(mapdata)       # go with maps - contains topo and geologic data
library(mapproj)       # for creating projected maps
library(sp)            # classes and methods for spatial data
library(maptools)      # tools for reading and handling spatial objects
library(gpclib)        # can't install because needs to be compiled...

library(sfsmisc)       # utilities from Seminar fuer Statistik ETH Zurich
library(raster)        # tools to deal with raster images
library(rgeos)         # interface to geometry engine
library(rgdal)         # bindings for the geospatial data abstraction library
library(scales)        # making transparency

library(ggmap)
library(ggplot2)
library(ggsn)         # for adding scale bars
library(directlabels) # for adding contour line labels
library(broom)        # instead of fortify in ggplot2 (recommended)

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

dpi <- 300
save.fig <- F
#dName <- "D:/TomosFolders/turtles/greens/SDBay/SDBayTelemetry/files_Oct2016/"
#dat.tmp <- read.csv(paste0(dName, "152322_DayNight_4hrs_inout_2016-10-03.csv"))
dat.tmp <- read.csv("data/152322_DayNight_4hrs_inout_2016-10-03.csv")
df.GPS <- na.omit(data.frame(lon = dat.tmp$Lon1,
                          lat = dat.tmp$Lat1,
                          Date = dat.tmp$Date,
                          DOY = YMD2DOY(dat.tmp$Date)))

#df.GPS <- subset(df1, LC == "GPS")
#df.ARGOS <- subset(df1, LC != "GPS")
df.Nov <- subset(df.GPS, (DOY>YMD2DOY("2015-10-31") &
                              DOY < YMD2DOY("2015-12-01")))
df.Dec <- subset(df.GPS, (DOY>YMD2DOY("2015-11-30") ))
df.Jan <- subset(df.GPS, (DOY>0 &
                            DOY < YMD2DOY("2016-02-01")))
df.Feb <- subset(df.GPS, (DOY>YMD2DOY("2016-01-31") &
                              DOY < YMD2DOY("2016-03-01")))

map.Nov <- get_map(location = c(lon = -116.9,
                                lat = 32.2),
                   zoom = 9,
                   maptype = "satellite",
                   color = "bw")
map.Nov.gg <- ggmap(map.Nov)

#map.small.gg
map.Nov.track <- map.Nov.gg +
  geom_point(data = df.Nov,
             aes(x = lon,
                 y = lat),
             color = "red",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Nov,
            aes(x = lon, y = lat),
            color = "red") +
  xlab("") +
  ylab("") +
  scalebar(data = df.Nov,
           dist = 20,
           height = 0.02,
           st.dist = 0.05,
           dd2km = TRUE,
           model = "WGS84",
           #location = "bottomright",
           st.size = 2,
           anchor = c(x=-116.1,y=31.6)) + # right end
  ggtitle("November")

#map.Nov.track
map.Dec <- get_map(location = c(lon = -117.2,
                                lat = 28.93),
                   zoom = 6,
                   maptype = "satellite",
                   color = "bw")
map.Dec.gg <- ggmap(map.Dec)

map.Dec.track <- map.Dec.gg +
  geom_point(data = df.Dec,
             aes(x = lon,
                 y = lat),
             color = "red",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Dec,
            aes(x = lon, y = lat),
            color = "red") +
  geom_point(data = df.Nov,
             aes(x = lon,
                 y = lat),
             color = "gold",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Nov,
            aes(x = lon, y = lat),
            color = "gold") +
  xlab("") +
  ylab("") +
  scalebar(data = df.Dec,
           dist = 100,
           height = 0.02,
           st.dist = 0.05,
           dd2km = TRUE,
           model = "WGS84",
           #location = "bottomright",
           st.size = 2,
           anchor = c(x=-111,y=24)) + # right end
  ggtitle("December")

#map.Dec.track
map.Jan <- get_map(location = c(lon = -114.7,
                                lat = 22),
                   zoom = 6,
                   maptype = "satellite",
                   color = "bw")
map.Jan.gg <- ggmap(map.Jan)

#map.small.gg
map.Jan.track <- map.Jan.gg +
  geom_point(data = df.Jan,
             aes(x = lon,
                 y = lat),
             color = "red",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Jan,
            aes(x = lon, y = lat),
            color = "red") +
  geom_point(data = df.Dec,
             aes(x = lon,
                 y = lat),
             color = "gold",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Dec,
            aes(x = lon, y = lat),
            color = "gold") +
  xlab("") +
  ylab("") +
  scalebar(data = df.Jan,
           dist = 100,
           height = 0.02,
           st.dist = 0.05,
           dd2km = TRUE,
           model = "WGS84",
           #location = "bottomright",
           st.size = 2,
           anchor = c(x=-109,y=17))  + # right end
  ggtitle("January")

#map.Jan.track

map.Feb <- get_map(location = c(lon = -114.73,
                                lat = 18.35),
                   zoom = 12,
                   maptype = "satellite",
                   color = "bw")
map.Feb.gg <- ggmap(map.Feb)

map.Feb.track <- map.Feb.gg +
  geom_point(data = df.Feb,
             aes(x = lon,
                 y = lat),
             color = "red",
             size = 1.6,
             alpha = 0.8) +
  geom_path(data = df.Feb,
            aes(x = lon, y = lat),
            color = "red") +
  geom_point(data = df.Jan,
             aes(x = lon,
                 y = lat),
             color = "gold",
             size = 1.6,
             alpha = 0.4) +
  geom_path(data = df.Jan,
            aes(x = lon, y = lat),
            color = "red") +
  xlab("") +
  ylab("") +
  scalebar(data = df.Jan,  # need to use df.Jan because df.Feb
           dist = 2,       # has not enough variability in latitude
           height = 0.0005,  # height gets almost zero.
           st.dist = 0.001,
           dd2km = TRUE,
           model = "WGS84",
           #location = "bottomright",
           st.size = 2,
           anchor = c(x=-114.63,y=18.30))  + # right end
  ggtitle("February")


if (save.fig){
  ggsave(filename = paste0(dName, "images/152322_Nov_DayNight_4hrs_2016-10-03.png"),
         plot = map.Nov.track, dpi = dpi)

  ggsave(filename = paste0(dName, "images/152322_Dec_DayNight_4hrs_2016-10-03.png"),
         plot = map.Dec.track, dpi = dpi)

  ggsave(filename = paste0(dName, "images/152322_Jan_DayNight_4hrs_2016-10-03.png"),
         plot = map.Jan.track, dpi = dpi)

  ggsave(filename = paste0(dName, "images/152322_Feb_DayNight_4hrs_2016-10-03.png"),
         plot = map.Feb.track, dpi = dpi)

}

# ggsave(filename = "images/152322_November.png",
#        plot = map.Nov.track,
#        dpi = dpi)
#
# ggsave(filename = "images/152322_December.png",
#        plot = map.Dec.track,
#        dpi = dpi)
#
# ggsave(filename = "images/152322_January.png",
#        plot = map.Jan.track,
#        dpi = dpi)
#
# ggsave(filename = "images/152322_February.png",
#        plot = map.Feb.track,
#        dpi = dpi)
#
