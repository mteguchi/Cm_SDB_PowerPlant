#Cm_SDB_movements

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

library(ggmap)
library(sp)
library(rgdal)         # bindings for the geospatial data abstraction library
library(broom)        # instead of fortify in ggplot2 (recommended)

internet <- T
if (internet){
  sdbay.all <- get_map(location = c(lon = -117.15,
                                    lat = 32.65),
                       zoom = 12,
                       maptype = "satellite",
                       color = "bw",
                       filename = 'sdbay_all',
                       force = F)
  saveRDS(sdbay.all, file = 'RData/sdbay_all.rds')

  sdbay.south <- get_map(location = c(lon = -117.117,
                                      lat = 32.625),
                         zoom = 14,
                         maptype = "satellite",
                         color = "bw",
                         filename = 'sdbay_south',
                         force = F)
  saveRDS(sdbay.south, file = 'RData/sdbay_south.rds')

  sdbay.med <- get_map(location = c(lon =  -117.117,
                                    lat = 32.625),
                       zoom = 13,
                       maptype = "satellite",
                       color = "bw",
                       filename = 'sdbay_med',
                       force = F)
  saveRDS(sdbay.med, file = 'RData/sdbay_med.rds')
} else {
  sdbay.all <- readRDS(file = 'RData/sdbay_all.rds')
  sdbay.south <- readRDS(file = 'RData/sdbay_south.rds')
  sdbay.med <- readRDS(file = 'RData/sdbay_med.rds')
}


map.sdbay.zm <- ggmap(sdbay.all)

map.sdbay.south <- ggmap(sdbay.south)

map.sdbay.med <- ggmap(sdbay.med)


# just use the 2014 data - there are old data files also
SDBay.eelg.2014 <- spTransform(readOGR(dsn = "GISfiles/Features",
                                       layer = "SD_Baywide_Eelgrass_2014_Final",
                                       verbose = F),
                               CRS("+proj=longlat +datum=WGS84"))
SDBay.eelg.2014.df <- tidy(SDBay.eelg.2014)

SDBay.eelg.2008 <- spTransform(readOGR(dsn = "GISfiles/Features",
                                       layer = "SD_Baywide_Eelgrass_2008",
                                       verbose = F),
                               CRS("+proj=longlat +datum=WGS84"))
SDBay.eelg.2008.df <- tidy(SDBay.eelg.2008)

SDBay <- read.csv("data/SDBay_polygon.csv")

