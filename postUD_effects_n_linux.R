
rm(list = ls())
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/tools/TomosFunctions.R'),
       source('~/R/tools/TomosFunctions.R'))

#library(readxl)
library(dplyr)
library(adehabitatHR)
library(rgdal)
library(leaflet) 
library(ggplot2)
library(lubridate)
library(tidyverse)

run.date2 <- "2018-10-31"

# internet <- F #TRUE
# save.figure <- T

# Minimum number of relocations per individual to be included in HR analysis
min_n <- 50
#N.end <- 32.69   # approx Coronado bridge
N.end <- 32.66 # changed from 32.69 because 152314 had a gap in data points between ~32.65 and ~32.66 - seems like there was a different behavior south and north of 32.66

source("HR_analysis_fcns.R")
tagprj <- readOGR(dsn = "Tag_065_UTMzone11n", 
                  layer = "tag_065_project")
tagproj <- proj4string(tagprj)
post.all <- read.csv("data/Post_GPS_LC_all_2018-09-18.csv")
pre.all <- read.csv("data/Pre_GPS_LC_all_2018-09-18.csv")

grid <- 1000

pre.kd <- HR.analysis.step1(min_n, pre.all, tagproj, grid = grid)
post.kd <- HR.analysis.step1(min_n, post.all, tagproj, grid = grid)

h.pre.1 <- find.h.adhoc(pre.kd$list.data$all.utm)
## Section 3.1.2
##Visually optimized hlim = c(0.565,1.5), grid=300
# grid values from 50 to 300 don't change the outcome;
# extent values from 1 to 200 don't change the outcome either

h.pre <- round(h.pre.1$h)

# pre.HR <- compute.area(h.pre, 
#                        pre.kd$list.data, 
#                        grid = grid)

post.summary <- read.csv("data/post_sample_summary_2018-10-31.csv")
ID.post.min_n <- filter(post.summary,
                       n.relocations > (min_n - 1))

pre.summary <- read.csv("data/pre_sample_summary_2018-10-31.csv")
ID.pre.min_n <- filter(pre.summary,
                       n.relocations > (min_n - 1))

#h.multiplier <-  seq(from = 0.1, to = 0.95, by = 0.05) 
# This loop takes a long time... It was run once already so start from 
# where it was left off. .RData file needs to be loaded to figure out
# how far along the first attempt was. 
extent <- 1
max.n <- dim(ID.post.min_n)[1] - 1
min.n <- nrow(ID.pre.min_n)
for (k1 in max.n:min.n){
  print(paste("k1 is ", k1, ": max is ", max.n))
  if (!file.exists(paste0("RData/areas_combos_n", k1, "_", run.date2, ".rds"))){
    combos <- combn(1:dim(ID.post.min_n)[1], k1)
    if (ncol(combos) > 1000){
      combos <- combos[, sample(1:ncol(combos), size = 1000)]
    }
    IDs <- ver.95.df.list <- pts.df.list <- vector(mode = "list", 
                                                   length = ncol(combos))
    area.95 <- area.50 <- lat.N <- vector(mode = "numeric", 
                                          length = ncol(combos))
    
    #p.list <- list()
    for (k in 1:ncol(combos)){
      ID.post.tmp <- data.frame(ArgosID = ID.post.min_n[combos[,k], "ArgosID"])
      post.tmp <- left_join(ID.post.tmp, post.all, by = "ArgosID")
      post.tmp.kd <- HR.analysis.step1(min_n, 
                                       post.tmp, 
                                       tagproj, 
                                       grid = grid)
      
      h.post.tmp <- find.h.adhoc(post.tmp.kd$list.data$all.utm,
                                 grid = grid, extent = extent)
      
      h.post <- round(h.post.tmp$h)
      
      post.HR <- compute.area(h.post, 
                              post.tmp.kd$list.data, 
                              grid = grid)
      
      tmp.HR <- HR.bvnorm.fcn(all.utm = post.tmp.kd$list.data$all.utm,
                              byID.utm = post.tmp.kd$list.data$byID.utm,
                              h = h.pre,
                              hlim=c(0.03, 1.5),
                              grid=grid, extent = extent)
      
      ver.95.df.list <- broom::tidy(spTransform(getverticeshr(tmp.HR$all.kd, 95),
                                                CRS("+proj=longlat")))
      
      pts.df.list <- data.frame(lon = post.tmp.kd$list.data$all.coords@coords[,1],
                                lat = post.tmp.kd$list.data$all.coords@coords[,2])
      
      # p.list[[k]] <- ggplot() +
      #   geom_polygon(data = tmp.df, aes(x = long, y = lat, group = group)) +
      #   geom_point(data = pts.df, aes(x = lon, y = lat),
      #              color = "green") + coord_map()
      
      area.50[k] <- tmp.HR$area.all["50"]
      area.95[k] <- tmp.HR$area.all["95"]
      lat.N[k] <- max(ver.95.df.list$lat)
      
      IDs[[k]] <- ID.post.tmp[,1]
    }
    
    areas.combos <- list(area95 = area.95,
                         area50 = area.50,
                         Nmost.area95 = lat.N,
                         ID = IDs,
                         polygon = ver.95.df.list,
                         points = pts.df.list,
                         combos = combos)
    
    saveRDS(areas.combos,
            file = paste0("RData/areas_combos_n", k1, "_", 
                          Sys.Date(), ".rds"))
  }
  
}


 
