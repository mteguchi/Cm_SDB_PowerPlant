# creating maps for San Diego Bay
# some code were originally taken from presentation by Kim Gilbert, UBC
# modified since

# Tomo Eguchi
# 16 June 2016

rm(list=ls())
library(maps)          # creating geographical maps
library(mapdata)       # go with maps - contains topo and geologic data
library(mapproj)       # for creating projected maps
library(sp)            # classes and methods for spatial data
library(maptools)      # tools for reading and handling spatial objects
library(gpclib)        # can't install because needs to be compiled... 5/18/2016
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

#library(png)

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))
D <- dirSelector()

# change it back to 300 for final
dpi <- 300

#sdmap <- get_map("San Diego", zoom = 10)  # this is default value
#p.z10 <- ggmap(sdmap)

sdbay.all <- get_map(location = c(lon = -117.17,
                                 lat = 32.65),
                    zoom = 12,
                    maptype = "satellite",
                    color = "bw")
map.sdbay.zm <- ggmap(sdbay.all)

sdbay.south <- get_map(location = c(lon = -117.117,
                                    lat = 32.625),
                       zoom = 14,
                       maptype = "satellite",
                       color = "bw")
map.sdbay.south <- ggmap(sdbay.south)
#map.sdbay.south

# just use the 2014 data - there are old data files also
SDBay.eelg.2014 <- spTransform(readOGR(dsn = "GISfiles/Features",
                                       layer = "SD_Baywide_Eelgrass_2014_Final",
                                       verbose = F),
                               CRS("+proj=longlat +datum=WGS84"))
SDBay.eelg.2014.df <- tidy(SDBay.eelg.2014)

# point data can be read just like any other flat files
data.files <- dir("data/", "_inout_2016-06-16.csv")

for (k in 1:length(data.files)){
  dat.tmp <- read.csv(file = paste0("Data/", data.files[k]))

    # let's try this with ggmap
  df1 <- na.omit(data.frame(lon = dat.tmp$Lon1,
                            lat = dat.tmp$Lat1,
                            LC = dat.tmp$LC,
                            inside = dat.tmp$inside))

  df2 <- df1[df1$inside == 1,]
  df.GPS <- subset(df2, LC == "GPS")
  df.ARGOS <- subset(df2, LC != "GPS")

  ID <- as.numeric(unlist(strsplit(data.files[k],
                                   '_DayNight_4hrs_inout_'))[1])
  if (ID == 152314 | ID == 152322){
    base.map <- map.sdbay.zm
    map.sdbay.zm.1 <- base.map +
      geom_point(data = df.GPS,
                 aes(x = lon,
                     y = lat),
                 color = "red",
                 size = 0.6,
                 alpha = 0.8) +
      geom_point(data = df.ARGOS,
                 aes(x = lon,
                     y = lat),
                 color = "yellow",
                 size = 0.7,
                 alpha = 0.8) +

      xlab("") +
      ylab("") +
      scalebar(data = df.GPS,
               dist = 2,
               height = 0.02,
               st.dist = 0.03,
               dd2km = TRUE,
               model = "WGS84",
               #location = "bottomright",
               st.size = 3,
               anchor = c(x=-117.08,y=32.57)) # right end
  } else {
    base.map <- map.sdbay.south
    map.sdbay.zm.1 <- base.map +
      geom_point(data = df.GPS,
                 aes(x = lon,
                     y = lat),
                 color = "red",
                 size = 0.6,
                 alpha = 0.8) +
      geom_point(data = df.ARGOS,
                 aes(x = lon,
                     y = lat),
                 color = "yellow",
                 size = 0.7,
                 alpha = 0.8) +
      xlab("") +
      ylab("") +
      scalebar(data = df.GPS,
               dist = 0.5,
               height = 0.02,
               st.dist = 0.03,
               dd2km = TRUE,
               model = "WGS84",
               #location = "bottomright",
               st.size = 3,
               anchor = c(x=-117.095,y=32.605)) # right end
  }

  # add the shapefile polygons here:
  # convert shapefiles into dataframe using tidy function in broom
  # it does the same thing as fortify in ggplot2


  map.sdbay.zm.eelg14 <- map.sdbay.zm.1 +
    geom_polygon(data = SDBay.eelg.2014.df,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill = "green",
                 alpha = 0.2) +
    ggtitle(ID)

  #map.sdbay.zm.eelg14
  ggsave(filename = paste0("images/",
                           unlist(strsplit(data.files[k], '_inout_'))[1],
                           "_",
                           unlist(strsplit(unlist(strsplit(data.files[1],
                                                           '_inout_'))[2],
                                           '.csv')), ".png"),
         plot = map.sdbay.zm.eelg14,
         dpi = dpi)
}



