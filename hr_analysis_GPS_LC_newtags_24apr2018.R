##San Diego Bay Turtle Movement Analysis
## Original R Code, JT Froeschke, December 29, 2015
## Data modified from previous versions by SE Graham, June 2016, May 2018

## Filtered data utilize GPS, and Argos LC = 1,2,3
## Filtered data do not include points on land
## Filtered data only allow for 1 relocation every 4 horus

## Purpose of the script is to compute homerange (area) using
## least squares cross-validation including 50% and 95% contours.
## An analysis of each turtle and an aggregate pre and post will be computed
## h values are chosen based on best judgment and gst behavior
rm(list = ls())
#getwd()
#list.files()

#Section 1: Load libraries and set wd
library(readxl)
library(dplyr)
library(adehabitatHR)
library(readxl)
library(rgdal)
library(leaflet)

#note: development version in use

##Section 2: read in data

## Section 2.1: Pre

d <- "data/files_Apr2018_withNewTags/"
tag37616 <-read.csv(paste0(d, "pre/37616_inside_DayNight_4hrs_2018-04-20.csv"))
tag37623 <-read.csv(paste0(d, "pre/37623_inside_DayNight_4hrs_2018-04-20.csv"))
tag44366 <-read.csv(paste0(d, "pre/44366_inside_DayNight_4hrs_2018-04-20.csv"))
tag52674 <-read.csv(paste0(d, "pre/52674_inside_DayNight_4hrs_2018-04-20.csv"))
tag52675 <-read.csv(paste0(d, "pre/52675_inside_DayNight_4hrs_2018-04-20.csv"))
tag78500 <-read.csv(paste0(d, "pre/78500_inside_DayNight_4hrs_2018-04-20.csv"))
tag79786 <-read.csv(paste0(d, "pre/79786_inside_DayNight_4hrs_2018-04-20.csv"))


##Section 2.2, r

 Pre.all <- rbind(tag79786,
                  tag78500,
                  tag52675,
                  tag52674,
                  tag37616,
                  tag37623,
                  tag44366)

write.csv(Pre.all, "outputs2/Pre_GPS_LC.all.csv", row.names=FALSE)
#View(Pre.all)

##Note I checked total of pre.all equals number of rows of individual tags
## e.g., 40+128+125+283+13+120+133+7

## Section 2.2: Post

tag12607106 <-read.csv(paste0(d, "post/12607106_inside_DayNight_4hrs_2018-04-20.csv"))
tag12607107 <-read.csv(paste0(d, "post/12607107_inside_DayNight_4hrs_2018-04-20.csv"))
tag126070 <-read.csv(paste0(d, "post/126070_inside_DayNight_4hrs_2018-04-20.csv"))
tag12606905 <-read.csv(paste0(d, "post/12606905_inside_DayNight_4hrs_2018-04-20.csv"))
tag12606907 <-read.csv(paste0(d, "post/12606907_inside_DayNight_4hrs_2018-04-20.csv"))
tag126068 <-read.csv(paste0(d, "post/126068_inside_DayNight_4hrs_2018-04-20.csv"))
tag126067 <-read.csv(paste0(d, "post/126067_inside_DayNight_4hrs_2018-04-20.csv"))
tag126066 <-read.csv(paste0(d, "post/126066_inside_DayNight_4hrs_2018-04-20.csv"))
tag126065 <-read.csv(paste0(d, "post/126065_inside_DayNight_4hrs_2018-04-20.csv"))
tag126064 <-read.csv(paste0(d, "post/126064_inside_DayNight_4hrs_2018-04-20.csv"))
tag44359 <-read.csv(paste0(d, "post/44359_inside_DayNight_4hrs_2018-04-20.csv"))
tag151375 <-read.csv(paste0(d, "new/151375_inside_DayNight_4hrs_2018-04-20.csv"))
tag151377 <-read.csv(paste0(d, "new/151377_inside_DayNight_4hrs_2018-04-20.csv"))
tag151378 <-read.csv(paste0(d, "new/151378_inside_DayNight_4hrs_2018-04-20.csv"))
tag151380 <-read.csv(paste0(d, "new/151380_inside_DayNight_4hrs_2018-04-20.csv"))
tag151381 <-read.csv(paste0(d, "new/151381_inside_DayNight_4hrs_2018-04-20.csv"))
tag151384 <-read.csv(paste0(d, "new/151384_inside_DayNight_4hrs_2018-04-20.csv"))
tag152313 <-read.csv(paste0(d, "new/152313_inside_DayNight_4hrs_2018-04-20.csv"))
tag152314 <-read.csv(paste0(d, "new/152314_inside_DayNight_4hrs_2018-04-20.csv"))
tag152315 <-read.csv(paste0(d, "new/152315_inside_DayNight_4hrs_2018-04-20.csv"))
tag152319 <-read.csv(paste0(d, "new/152319_inside_DayNight_4hrs_2018-04-20.csv"))
tag152322 <-read.csv(paste0(d, "new/152322_inside_DayNight_4hrs_2018-04-20.csv"))
tag152323 <-read.csv(paste0(d, "new/152323_inside_DayNight_4hrs_2018-04-20.csv"))


Post.all <- rbind(tag12607106,
                  tag12607107,
                  tag126070,
                  tag12606905,
                  tag12606907,
                  tag126068,
                  tag126067,
                  tag126066,
                  tag126065,
                  tag126064,
                  tag44359,
                  tag151375,
                  tag151377,
                  tag151378,
                  tag151380,
                  tag151381,
                  tag151384,
                  tag152313,
                  tag152314,
                  tag152315,
                  tag152319,
                  tag152322,
                  tag152323)

write.csv(Post.all, "outputs2/Post_GPS_LC.all.csv", row.names=FALSE)

##note: can ignore warnings
#View(Post.all)


## Section 3: Compute HR models
## Section 3.1: Pre
## get coordinates as a dataframe and make a spatial object
Pre.all.coords <- data.frame(x=Pre.all$Lon, y=Pre.all$Lat)
coordinates(Pre.all.coords) <- ~ x + y
class(Pre.all.coords)
plot(Pre.all.coords, axes=TRUE) ## sanity check

Post.all.coords <- data.frame(x=Post.all$Lon, y=Post.all$Lat)
#Post.all.coords <- subset(Post.all.coords, y < 32.66)
coordinates(Post.all.coords) <- ~ x + y
class(Post.all.coords)
plot(Post.all.coords, axes=TRUE) ## sanity check
##Section 3.1.2: Project data
## Import previous file to get projection
## Project sample data following this example
## http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html
## use spTransform to convert new data to tag example projection
library(rgdal)

#tagprj <- readOGR("/Users/sgraham/R/gst_hr_analysis2018/Tag_065_UTMzone11n", "tag_065_project")
tagprj <- readOGR("Tag_065_UTMzone11n", "tag_065_project")

plot(tagprj, axes=TRUE)
saveproj <- proj4string(tagprj)
Pre.all.proj <- Pre.all.coords
latlong = "+init=epsg:4326"
proj4string(Pre.all.proj) = CRS(latlong)
Pre.all.utm <- spTransform(Pre.all.proj, saveproj)
plot(Pre.all.utm, axes=TRUE) ## sanity check

Post.all.proj <- Post.all.coords
latlong = "+init=epsg:4326"
proj4string(Post.all.proj) = CRS(latlong)
Post.all.utm <- spTransform(Post.all.proj, saveproj)
plot(Post.all.utm, axes=TRUE) ## sanity check


## Section 3.1.2
##Visually optimized hlim = c(0.565,1.5), grid=300
Pre.kd <- kernelUD(Pre.all.utm, h="LSCV",  hlim = c(0.03, 1.5), grid=300)
plotLSCV(Pre.kd)
Pre.Area <- kernel.area(Pre.kd, percent = seq(20, 95, by = 5),
                        unin = c("m"),
                        unout = c("km"), standardize = FALSE)
Pre.Area
##start here:
Pre.bw <- Pre.kd@h[[3]] ##bandwidth estimate
Pre.Area.50 <- round(Pre.Area[7],2)
Pre.Area.95 <- round(Pre.Area[16],2)

##repeat for each and export data: bw, 50, 95, and plot

## Section 3.1.3: Plot and export
library(png)
Pre.ver.50 <- getverticeshr(Pre.kd, 50)
Pre.ver.95 <- getverticeshr(Pre.kd, 95)

plot(Pre.ver.50)
plot(Pre.ver.95)

png(filename="plots2/Pre.all.png")
plot(Pre.ver.95, axes=TRUE#, main=paste("Pre.all ", "Area = ", Area.95, " km2\n", "bandwidth = ", round(Pre.bw,1), sep="")
     )
plot(getverticeshr(Pre.kd, 95), add=TRUE, lwd=2)
plot(Pre.all.utm, add=TRUE, col="blue")
dev.off()

##Write shapefile
#Empty shapefile folder
writeOGR(Pre.ver.50, "shapefiles2/Pre.ver.50.shp",layer="Pre.ver.50", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(Pre.ver.95, "shapefiles2/Pre.ver.95.shp",layer="Pre.ver.95", driver="ESRI Shapefile", overwrite_layer=TRUE)


###get projection of Tag_78500.bw.id
##reproject points
#proj4string(Pre.all) <- proj4string(all3)
Pre.leafpoints <-spTransform(Pre.all.utm,CRS("+proj=longlat"))

##reproject vertices
proj4string(Pre.ver.50) <- proj4string(Pre.all.utm)
Pre.leafver.50 <-spTransform(Pre.ver.50,CRS("+proj=longlat"))

proj4string(Pre.ver.95) <- proj4string(Pre.all.utm)
Pre.leafver.95 <-spTransform(Pre.ver.95,CRS("+proj=longlat"))
library(leaflet)
m <- leaflet() %>% setView(lng = -117.1, lat = 32.65, zoom = 11)
m %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(data=Pre.leafver.95, stroke=TRUE, color="#2ca25f", fillOpacity=0.75, group="Home range 95%")%>%
  addPolygons(data=Pre.leafver.50, stroke=TRUE, color="#99d8c9", fillOpacity=0.75, group="Home range 50%")%>%
  addCircles(data=Pre.leafpoints, group="Location data", color="yellow", fillOpacity=0.3, stroke=FALSE) %>%
  addLayersControl(
    overlayGroups = c("Home range 95%", "Home range 50%", "Location data"),
    options = layersControlOptions(collapsed = FALSE)
  )



Pre.outputs <- data.frame(Period="Pre", Source="All", Method="LSCV", Bandwidth=Pre.bw, A50=Pre.Area.50, A95=Pre.Area.95)
write.csv(Pre.outputs, "outputs2/PreOutputs.csv", row.names=FALSE)



############Post
## Section 3.2.2
#hlim and grid visually optimized = 0.525/375 m
Post.kd <- kernelUD(Post.all.utm, h="LSCV",  hlim = c(.35, 1.5), grid=175) #set grid=100
plotLSCV(Post.kd)
Post.Area <- kernel.area(Post.kd, percent = seq(20, 95, by = 5),
                         unin = c("m"),
                         unout = c("km"), standardize = FALSE)
Post.Area
##start here:
Post.bw <- Post.kd@h[[3]] ##bandwidth estimate
Post.Area.50 <- round(Post.Area[7],2)
Post.Area.95 <- round(Post.Area[16],2)

##repeat for each and export data: bw, 50, 95, and plot

## Section 3.2.3: Plot and export
Post.ver.50 <- getverticeshr(Post.kd, 50)
Post.ver.95 <- getverticeshr(Post.kd, 95)

plot(Post.ver.50)
plot(Post.ver.95)

png(filename="plots2/Post.all.png")
plot(Post.ver.95, axes=TRUE#, main=paste("Post.all ", "Area = ", Area.95, " km2\n", "bandwidth = ", round(Post.bw,1), sep="")#
     )
plot(getverticeshr(Post.kd, 95), add=TRUE, lwd=2)
plot(Post.all.utm, add=TRUE, col="blue")
dev.off()

##Write shapefile
writeOGR(Post.ver.50, "shapefiles2/Post.ver.50.shp",layer="Post.ver.50", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(Post.ver.95, "shapefiles2/Post.ver.95.shp",layer="Post.ver.95", driver="ESRI Shapefile", overwrite_layer=TRUE)


Post.outputs <- data.frame(Period="Post", Source="All", Method="LSCV", Bandwidth=Post.bw, A50=Post.Area.50, A95=Post.Area.95)
write.csv(Post.outputs, "outputs2/PostOutputs.csv", row.names=FALSE)


###get projection of Tag_78500.bw.id
##reproject points
#proj4string(Post.all) <- proj4string(all3)
Post.leafpoints <-spTransform(Post.all.utm,CRS("+proj=longlat"))

##reproject vertices
proj4string(Post.ver.50) <- proj4string(Post.all.utm)
Post.leafver.50 <-spTransform(Post.ver.50,CRS("+proj=longlat"))

proj4string(Post.ver.95) <- proj4string(Post.all.utm)
Post.leafver.95 <-spTransform(Post.ver.95,CRS("+proj=longlat"))
library(leaflet)
m <- leaflet() %>% setView(lng = -117.1, lat = 32.65, zoom = 11)
m %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(data=Post.leafver.95, stroke=TRUE, color="#2ca25f", fillOpacity=0.75, group="Home range 95%")%>%
  addPolygons(data=Post.leafver.50, stroke=TRUE, color="#99d8c9", fillOpacity=0.75, group="Home range 50%")%>%
  addCircles(data=Post.leafpoints, group="Location data", color="yellow", fillOpacity=0.3, stroke=FALSE) %>%
  addLayersControl(
    overlayGroups = c("Home range 95%", "Home range 50%", "Location data"),
    options = layersControlOptions(collapsed = FALSE)
  )






## Section 3.2.4: leaflet

###get projection of Tag_78500.bw.id
##reproject points
#proj4string(Post.all) <- proj4string(all3)
Post.leafpoints <-spTransform(Post.all.utm,CRS("+proj=longlat"))



############## end


##Section 4: Iterate through each tag in a loop
#outputs: 50 and 95 shapefiles and spreadsheet
# ##Data sources
# Pre.all.utm
# Post.all.utm

## get coordinates as a dataframe and make a spatial object
#bind a new set of pretags that does not include small relocation values for turtles ##called pre.some
Pre.some <- rbind(tag78500,
                 tag52675,
                 tag37616,
                 tag37623,
                 tag44366)

Pre.unique <- unique(Pre.some$ArgosID)
Pre.ind.outputs <- c() #to hold results

###clear all files from output folders before running new script

for(i in 1:length(Pre.unique)){
  Pre.tmp <- subset(Pre.some, ArgosID==Pre.unique[i])
  print(i)


  Pre.tmp.coords <- data.frame(x=Pre.tmp$Lon, y=Pre.tmp$Lat)
  coordinates(Pre.tmp.coords) <- ~ x + y
  class(Pre.tmp.coords)
  #plot(Pre.tmp.coords, axes=TRUE) ## sanity check


  # library(rgdal)
  # tagprj <- readOGR("C:/Users/Costco/Documents/Dropbox/John Business/Business/sdbay/homerange/shps/Tag_065.shp",
  #                   layer="Tag_065")
  #plot(tagprj, axes=TRUE)
  #saveproj <- proj4string(tagprj)
  Pre.tmp.proj <- Pre.tmp.coords
  #latlong = "+init=epsg:4326"
  proj4string(Pre.tmp.proj) = CRS(latlong)
  Pre.tmp.utm <- spTransform(Pre.tmp.proj, saveproj)
  #plot(Pre.tmp.utm, axes=TRUE) ## sanity check


  ## Section 4.1.2
  Pre.tmp.kd <- kernelUD(Pre.tmp.utm, h="LSCV",  hlim = c(0.565, 1.5), grid=125) #set values from pre.all
  #plotLSCV(Pre.kd)
  Pre.tmp.Area <- kernel.area(Pre.tmp.kd, percent = seq(20, 95, by = 5),
                              unin = c("m"),
                              unout = c("km"), standardize = FALSE)
  Pre.tmp.Area
  ##start here:
  Pre.tmp.bw <- Pre.tmp.kd@h[[3]] ##bandwidth estimate
  Pre.tmp.Area.50 <- round(Pre.tmp.Area[7],2)
  Pre.tmp.Area.95 <- round(Pre.tmp.Area[16],2)

  Pre.ver.tmp.50 <- getverticeshr(Pre.tmp.kd, 50)
  Pre.ver.tmp.95 <- getverticeshr(Pre.tmp.kd, 95)


  filename50 <- paste("shapefiles2/", "tag",Pre.unique[i],"percent50th", ".shp", sep="")
  filename95 <- paste("shapefiles2/", "tag",Pre.unique[i],"percent95th", ".shp", sep="")
  ##Write shapefile
  writeOGR(Pre.ver.tmp.50, filename50,layer="Pre.ver.tmp.50", driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeOGR(Pre.ver.tmp.95, filename95,layer="Pre.ver.tmp.95", driver="ESRI Shapefile", overwrite_layer=TRUE)


  pre.tmp.outputs <- data.frame(Period="Pre", Source=Pre.unique[i], Method="LSCV",
                                Bandwidth=Pre.tmp.bw,
                                A50=Pre.tmp.Area.50, A95=Pre.tmp.Area.95)

  Pre.ind.outputs <- rbind(Pre.ind.outputs, pre.tmp.outputs)
}

write.csv(Pre.ind.outputs , "outputs2/Pre.ind.outputs .csv", row.names=FALSE)


###########
## Section 5: Post



## get coordinates as a dataframe and make a spatial object
Post.unique <- unique(Post.all$ArgosID)
Post.ind.outputs <- c() #to hold results

for(i in 1:length(Post.unique)){
  Post.tmp <- subset(Post.all, ArgosID==Post.unique[i])
  print(i)


  Post.tmp.coords <- data.frame(x=Post.tmp$Lon, y=Post.tmp$Lat)
  coordinates(Post.tmp.coords) <- ~ x + y
  class(Post.tmp.coords)
  #plot(Post.tmp.coords, axes=TRUE) ## sanity check


  # library(rgdal)
  # tagprj <- readOGR("C:/Users/Costco/Documents/Dropbox/John Business/Business/sdbay/homerange/shps/Tag_065.shp",
  #                   layer="Tag_065")
  #plot(tagprj, axes=TRUE)
  #saveproj <- proj4string(tagprj)
  Post.tmp.proj <- Post.tmp.coords
  #latlong = "+init=epsg:4326"
  proj4string(Post.tmp.proj) = CRS(latlong)
  Post.tmp.utm <- spTransform(Post.tmp.proj, saveproj)
  #plot(Post.tmp.utm, axes=TRUE) ## sanity check

  ## Section 4.1.2
  Post.tmp.kd <- kernelUD(Post.tmp.utm, h="LSCV",  hlim = c(0.525, 1.5), grid=375) #set grid=300
  #plotLSCV(Post.kd)
  Post.tmp.Area <- kernel.area(Post.tmp.kd, percent = seq(20, 95, by = 5),
                               unin = c("m"),
                               unout = c("km"), standardize = FALSE)
  Post.tmp.Area
  ##start here:
  Post.tmp.bw <- Post.tmp.kd@h[[3]] ##bandwidth estimate
  Post.tmp.Area.50 <- round(Post.tmp.Area[7],2)
  Post.tmp.Area.95 <- round(Post.tmp.Area[16],2)

  Post.ver.tmp.50 <- getverticeshr(Post.tmp.kd, 50)
  Post.ver.tmp.95 <- getverticeshr(Post.tmp.kd, 95)


  filename50 <- paste("shapefiles2/", "tag",Post.unique[i],"percent50th", ".shp", sep="")
  filename95 <- paste("shapefiles2/", "tag",Post.unique[i],"percent95th", ".shp", sep="")
  ##Write shapefile
  writeOGR(Post.ver.tmp.50, filename50,layer="Post.ver.tmp.50", driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeOGR(Post.ver.tmp.95, filename95,layer="Post.ver.tmp.95", driver="ESRI Shapefile", overwrite_layer=TRUE)


  Post.tmp.outputs <- data.frame(Period="Post", Source=Post.unique[i], Method="LSCV",
                                 Bandwidth=Post.tmp.bw,
                                 A50=Post.tmp.Area.50, A95=Post.tmp.Area.95)

  Post.ind.outputs <- rbind(Post.ind.outputs, Post.tmp.outputs)
}

write.csv(Post.ind.outputs , "outputs/Post.ind.outputs .csv", row.names=FALSE)







##combine data
## dt table
## leaflet
Pre.outputs2 <- Pre.outputs
Pre.outputs2$Source <- factor(Pre.outputs2$Source)
Pre.ind.outputs2 <- Pre.ind.outputs
Pre.ind.outputs2$Source <- factor(Pre.ind.outputs2$Source)

Post.outputs2 <- Post.outputs
Post.outputs2$Source <- factor(Post.outputs2$Source)
Post.ind.outputs2 <- Post.ind.outputs
Post.ind.outputs2$Source <- factor(Post.ind.outputs2$Source)



summary.all <- rbind(Pre.outputs2,Pre.ind.outputs2, Post.outputs2, Post.ind.outputs2)

write.csv(summary.all , "outputs2/summaryall.csv", row.names=FALSE)



save.image("homerangegpsLC.RData")

##Post all leaflet map
setwd("/Users/sgraham/R/gst_hr_analysis2016")
#load("homerange5.RData")
library(leaflet)
m <- leaflet() %>% setView(lng = -117.1, lat = 32.65, zoom = 11)
m %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(data=Post.leafver.95, stroke=TRUE, color="#2ca25f", fillOpacity=0.75, group="Post home range 95%")%>%
  addPolygons(data=Post.leafver.50, stroke=TRUE, color="#99d8c9", fillOpacity=0.75, group="Post home range 50%")%>%
  addCircles(data=Post.leafpoints, group="Location data", color="yellow", fillOpacity=0.3, stroke=FALSE) %>%
  addLayersControl(
    overlayGroups = c("Post home range 95%", "Post home range 50%", "Location data"),
    options = layersControlOptions(collapsed = FALSE)
  )
