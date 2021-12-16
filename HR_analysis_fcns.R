#HR_analysis_functions

library(dplyr)
library(adehabitatHR)
library(rgdal)
library(leaflet)
library(ncdf4)


# I'm not sure what this does - probably defines lat-lon coordinate system?
latlong <- "+init=epsg:4326"

# a function to create a data frame with minimum number of data points per individual
# then convert geographic coordinates so HR can be computed. Creates a list of length
# four. tagproj is the projection definition, which is defined at the end of next chunk

make.HR.dataset <- function(data.all, ID.min_n, tagproj){
  selected <- right_join(data.all,
                         ID.min_n,
                         by = "ArgosID")

  all.coords <- data.frame(x=selected$Lon1,
                           y=selected$Lat1)

  all.time <- data.frame(local = selected$LocalDateTime,
                         UTC = selected$UTCDateTime)
  
  coordinates(all.coords) <- ~ x + y
  proj4string(all.coords) <- CRS(latlong)
  all.utm <- spTransform(all.coords, tagproj)

  byID.coords <- data.frame(x=selected$Lon1,
                            y=selected$Lat1,
                            ID = as.factor(selected$ArgosID))

  byID.time <- data.frame(UTC = selected$UTCDateTime,
                          local = selected$LocalDateTime)
  
  coordinates(byID.coords) <- ~ x + y
  proj4string(byID.coords) <- CRS(latlong)
  byID.utm <- spTransform(byID.coords, tagproj)

  eachID.utm <- eachID.coords <- list()
  eachID.time <- list()

  unique.ID <- unique(selected$ArgosID)
  for (k in 1:length(unique.ID)){
    tmp.data <- filter(selected, ArgosID == unique.ID[k])
    tmp1 <- data.frame(x = tmp.data$Lon1,
                       y = tmp.data$Lat1)

    coordinates(tmp1) <- ~ x + y
    proj4string(tmp1) <- CRS(latlong)
    tmp2 <- spTransform(tmp1, tagproj)

    eachID.coords[[k]] <- tmp1
    eachID.utm[[k]] <- tmp2
    eachID.time[[k]] <- data.frame(UTC = tmp.data$UTCDateTime,
                                   local = tmp.data$LocalDateTime)
  }

  return(list(all.coords = all.coords,
              all.utm = all.utm,
              all.time = all.time,
              byID.coords = byID.coords,
              byID.utm = byID.utm,
              byID.time = byID.time,
              eachID.coords = eachID.coords,
              eachID.utm = eachID.utm,
              eachID.time = eachID.time,
              unique.ID = unique.ID))
}

# A function to compute UDs using all data and for each individual. Returns a list
# of estimated bandwidth, kernel density estimates as well as areas
# for 50 and 95% UDs - removed byID output because h values need to be changed
# for each individual to compute the most appropriate HRs (2019-11-04). 

HR.bvnorm.fcn <- function(all.utm,
                          h= "href",
                          hlim=c(0.03, 1.5),
                          grid=1000, extent = 1){

  all.kd <- kernelUD(all.utm,
                     h = h,
                     hlim = hlim,
                     grid = grid,
                     extent = extent,
                     kern = "bivnorm")

  Area.all <- kernel.area(all.kd,
                          percent = c(50, 95),
                          unin = c("m"),
                          unout = c("km2"),
                          standardize = FALSE)

  # byID.kd <- kernelUD(byID.utm,
  #                     h = all.kd@h$h ,
  #                     hlim = hlim,
  #                     grid = grid,
  #                     extent = extent,
  #                     kern = "bivnorm")
  # 
  # Area.byID <- kernel.area(byID.kd,
  #                          percent = c(50, 95),
  #                          unin = c("m"),
  #                          unout = c("km2"),
  #                          standardize = FALSE)

  bw <- all.kd@h$h ##bandwidth estimate
  #Area.50 <- Area.all["50"]
  #Area.95 <- Area.all["95"]

  #ver.50 <- getverticeshr(all.kd, 50)
  #ver.95 <- getverticeshr(all.kd, 95)

  return(list(bw = bw,
              all.kd = all.kd,
              #byID.kd = byID.kd,
              area.all = Area.all))
              #area.byID = Area.byID))
}

# A function to conduct the first step of UD analysis. It takes a full dataset
# and select individuals with at least min_n (50) data points. Computes UD using the
# reference bandwidth (href) and the method of least square cross validation (LSCV).
# UD from the LSCV method can be plotted to find the bandwidth that minimizes the
# CV of bandwidth.
HR.analysis.step1 <- function(min_n = 50, data.df, tagproj, grid=1000){
  data.df %>% count(by = ArgosID) %>%
    filter(n > (min_n - 1)) %>%
    #dplyr::select(by) %>%
    rename(ArgosID = by) -> ID.min_n_day

  list.data <- make.HR.dataset(data.df, ID.min_n_day, tagproj)

  kd.href <- kernelUD(list.data$all.utm,
                      h="href",
                      grid=grid,
                      kern = "bivnorm")

  kd.LSCV <- kernelUD(list.data$all.utm,
                      h="LSCV",
                      hlim = c(0.01, 5.5),
                      grid = grid,
                      kern = "bivnorm")
  
  return(list(kd.href = kd.href,
              kd.LSCV = kd.LSCV,
              IDs = ID.min_n_day,
              list.data = list.data))
}

# A function to conduct home range analysis for a wide range of bandwidth values.
# INputs are multiplier values for the reference bandwidth, e.g., seq(from = 0.05,
# to = 0.95, by = 0.05), estimated reference bandwidth (found in the output of
# HR.analysis.step1), an output of make.HR.dataset, and grid size (default is 300)
# It returns a figure (faceted 95% UDs in SDB), a dataframe used to make the figure,
# and bandwidth values that correspond to mulipliers.
HR.analysis <- function(h.multiplier, kd.href, data.list, grid = 1000){
  h.adhoc <- h.multiplier * kd.href@h$h

  kd.adhoc <- vector(mode = "list", length = length(h.adhoc))
  for (k in 1:length(h.adhoc)){
    kd.adhoc[[k]] <- kernelUD(data.list$all.utm,
                              h = h.adhoc[k],
                              grid = grid,
                              kern = "bivnorm")

  }

  # make dataframes of vertices:
  ver.95.adhoc <- lapply(kd.adhoc,
                         FUN = getverticeshr,
                         percent = 95, unin = "m", unout = "km2")

  ver.95.adhoc.tmp <- lapply(ver.95.adhoc,
                             FUN = spTransform,
                             CRS = CRS("+proj=longlat"))

  ver.95.adhoc.list <- lapply(ver.95.adhoc.tmp,
                              FUN = broom::tidy)

  for (k in 1:length(ver.95.adhoc.list)){
    ver.95.adhoc.list[[k]] <- ver.95.adhoc.list[[k]] %>%
      mutate(h = h.adhoc[k]) %>%
      mutate(h.fac = as.factor(round(h, digits = 2))) %>%
      mutate(h.multip = h.multiplier[k])
  }

  # rbind all:
  ver.95.adhoc.df <- do.call("rbind", ver.95.adhoc.list)

  p.h.adhoc <- ggplot(data = ver.95.adhoc.df,
                      aes(x = long, y = lat,
                          group = group)) +
    geom_polygon() +
    coord_map() + facet_grid(. ~ h.multip) +
    facet_wrap( ~ h.multip, nrow = 3) +
    labs(x = "", y = "")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(breaks = c(-117.14, -117.12, -117.10),
                       limits = c(-117.14, -117.09))+
    scale_y_continuous(breaks = c(32.62, 32.64, 32.66),
                       limits = c(32.59, 32.66))

  return(list(figure = p.h.adhoc,
              ver.95 = ver.95.adhoc.df,
              h = h.adhoc))
}

# A function to compute 50 and 95% UD areas. INputs are bandwidth (h),
# the output from make.HR.dataset, and grid value. It uses HR.bvnorm.fcn.
# Returns 50 and 95% areas and vertices, which are used to plot UDs.
# removed byID output because I changed the HR.bvnorm.fcn() above (2019-11-04)
compute.area <- function(h, all.utm, grid=1000){
  #h.multiplier * kd.href@h$h
  
  HR <- HR.bvnorm.fcn(all.utm$all.utm,
                      #list.data$byID.utm,
                      h = h,
                      hlim=c(0.03, 1.5),
                      grid=grid, extent = 1)
  bw <- HR$all.kd@h$h ##bandwidth estimate

  return(list(area.50 = HR$area.all["50"],
              area.95 = HR$area.all["95"],
              ver.50 = getverticeshr(HR$all.kd, 50, unin = "m", unout = "km2"),
              ver.95 = getverticeshr(HR$all.kd, 95, unin = "m", unout = "km2")))
              # area.byID.50 = HR$area.byID["50",],
              # area.byID.95 = HR$area.byID["95",],
              # ver.byID.50 = getverticeshr(HR$byID.kd, 50),
              # ver.byID.95 = getverticeshr(HR$byID.kd, 95)))
}

# This funciton finds a bandwidth value that makes 95% UD contiguous
find.h.adhoc <- function(utm.data, grid = 1000, extent = 1){
  tmp.href <- kernelUD(utm.data, h = "href",
                       kern = "bivnorm", grid = grid,
                       extent = extent)
  h1 <- tmp.href@h$h
  h.multip <- 0.05
  n.seg <- 2
  k <- 1
  while (n.seg > 1){
    h <- h1 * h.multip
    tmp.UD <- kernelUD(utm.data, h = h,
                       kern = "bivnorm", grid = grid,
                       extent = extent)
    tmp.V <- getverticeshr(tmp.UD, 95, unin = "m", unout = "km2")
    n.seg <- length(tmp.V@polygons[[1]]@Polygons)
    h.multip <- h.multip + 0.05
  }
  return(list(v.95 = tmp.V,
              UD.95 = tmp.UD,
              h = h,
              href = h1,
              h.multip = h.multip - 0.05))
}

# This funciton finds a bandwidth value that makes 95% UD contiguous
# Tried to use optimize function but it's not working... 
find.h.optim <- function(utm.data, grid = 1000, extent = 1){

  tmp.href <- kernelUD(utm.data, h = "href",
                       kern = "bivnorm", grid = grid,
                       extent = extent)
  h1 <- tmp.href@h$h
  
  f <- function(x, h1, utm.data, grid = grid, extent = extent){
    h <- h1 * x
    tmp.UD <- kernelUD(utm.data, 
                       h = h,
                       kern = "bivnorm", 
                       grid = grid,
                       extent = extent)
    tmp.V <- getverticeshr(tmp.UD, 95, unin = "m", unout = "km2")
    length(tmp.V@polygons[[1]]@Polygons)
    
  }

  h.multip <- optimize(f = f, interval = c(0,1),
                       h1 = h1, utm.data = utm.data, 
                       grid = grid, extent = extent)
  
  tmp.UD <- kernelUD(utm.data, h = h1 * h.multip.tmp,
                     kern = "bivnorm", 
                     grid = grid,
                     extent = extent)
  tmp.V <- getverticeshr(tmp.UD, 95, unin = "m", unout = "km2")
    
  return(list(v.95 = tmp.V,
              UD.95 = tmp.UD,
              h = h1 * h.multip,
              href = h1,
              h.multip = h.multip))
}

# Download or open files that contain data from buoys. It downloads in
# .nc and .csv formats. 
getSDwtmp<- function(begindate, enddate,
                     outdir = "data/"){
  #dt <- as.Date(enddate) - as.Date(begindate)
  #if (dt > 30) stop('Time range needs to be less than or equal to 30.')
  
  outfilename.nc <- paste0(outdir, "nc/Wtmp_SDBay_",
                        begindate, '_', enddate, ".nc")
  outfilename.csv <- paste0(outdir, "csv/Wtmp_SDBay_",
                           begindate, '_', enddate, ".csv")
  
  url.nc <- paste0("https://upwell.pfeg.noaa.gov/erddap/tabledap/cwwcNDBCMet.nc?station%2Clongitude%2Clatitude%2Ctime%2Catmp%2Cwtmp%2Ctide&station%3E=%22SDBC1%22&time%3E=",
                 begindate, "T00%3A00%3A00Z&time%3C=",
                 enddate, "T20%3A00%3A00Z")
  url.csv <- paste0("https://upwell.pfeg.noaa.gov/erddap/tabledap/cwwcNDBCMet.csv?station%2Clongitude%2Clatitude%2Ctime%2Catmp%2Cwtmp%2Ctide&station%3E=%22SDBC1%22&time%3E=",
                   begindate, "T00%3A00%3A00Z&time%3C=", 
                   enddate, "T20%3A00%3A00Z")
  
  if (!file.exists(outfilename.nc)){

    download.file(url.nc,
                  destfile = outfilename.nc,
                  mode='wb')

  }
  
  if (!file.exists(outfilename.csv)){
    
    download.file(url.csv,
                  destfile = outfilename.csv,
                  mode='wb')
    
  }
  
  outfile.nc.info <- file.info(outfilename.nc)
  outfile.csv.info <- file.info(outfilename.csv)
  
  out.list <- list(url = list(csv = url.csv, nc = url.nc),
                   filename = list(csv = outfilename.csv, nc = outfilename.nc),
                   file.info = list(csv = outfile.csv.info, nc = outfile.nc.info))
  
  return(out.list)
}

tmp.summary <- function(begin.date, end.date){
  # nc format doesn't seem to work --- gets error using nc_open
  # 6 November 2018
  #datafileID <- nc_open(tmp.nc$filename)
  
  # these files have column headings in the first row and their
  # units in the second row. so, first two rows need to be removed
  
  tmp.data <- getSDwtmp(begin.date, end.date,
                        outdir = "data/Wtmp_SDBay/")
  col_names <- names(read_csv(tmp.data$filename$csv, n_max = 0))
  data.tmp <- read_csv(file = tmp.data$filename$csv,
                       col_names = col_names,
                       skip = 2) %>%
    filter(station == "SDBC1")
  
  wtemp <- data.tmp %>%
    summarise(mean.wtmp = mean(as.numeric(wtmp), na.rm = TRUE))
  min_wtemp <- data.tmp %>%
    summarise(min.wtmp = min(as.numeric(wtmp), na.rm = TRUE))
  max_wtemp <- data.tmp %>%
    summarise(max.wtmp = max(as.numeric(wtmp), na.rm = TRUE))
  SD_wtemp <- data.tmp %>%
    summarise(SD.wtmp = sd(as.numeric(wtmp), na.rm = TRUE))
  
  return(list(mean = wtemp, min = min_wtemp,
              max = max_wtemp, SD = SD_wtemp))
}

# extracts KD for each ID given the total KD
UD.eachID <- function(kd.all, grid.value = 1000){
  UD.all <- UD.95 <- UD.75 <- UD.50 <- vector(mode = "list", 
                                              length = length(kd.all$list.data$eachID.utm))
  
  h.eachID <- h.multip.eachID <- vector(mode = "numeric", 
                                        length = length(kd.all$list.data$eachID.utm))
  UD.area <- data.frame(ID = NA, area.50 = NA, 
                        area.75 = NA, area.95 = NA)
  #UD.area <- vector(mode = "list", 
  #                  length = length(kd.all$list.data$eachID.utm))
  k <- 1
  for (k in 1:length(kd.all$list.data$eachID.utm)){
    
    dat.utm <- kd.all$list.data$eachID.utm[[k]]
    best.h <- find.h.adhoc(dat.utm)
    h.eachID[k] <- ceiling(best.h$h)
    h.multip.eachID[k] <- best.h$h.multip
    UD <- kernelUD(dat.utm, 
                   h = h.eachID[k], 
                   kern = "bivnorm", 
                   grid = grid.value)
    
    # remove land
    UD.95[[k]] <- get.area.UTM(getverticeshr(UD, 95, unin = "m", unout = "km2"))
    UD.75[[k]] <- get.area.UTM(getverticeshr(UD, 75, unin = "m", unout = "km2")) 
    UD.50[[k]] <- get.area.UTM(getverticeshr(UD, 50, unin = "m", unout = "km2"))
    UD.all[[k]] <- UD
    
    # UD.tmp <- kernel.area(UD,
    #                       percent = c(50, 75, 95),
    #                       unin = c("m"),
    #                       unout = c("km2"),
    #                       standardize = FALSE)
    
    # UD.area[k,] <- c("ID" = kd.all$list.data$unique.ID[k], 
    #                  "area.50" =  UD.tmp[1],
    #                  "area.75" = UD.tmp[2],
    #                  "area.95" = UD.tmp[3])
    
    UD.area[k,] <- c("ID" = kd.all$list.data$unique.ID[k], 
                     "area.50" = slot(UD.50[[k]]@polygons[[1]], "area")/1000000,
                     "area.75" = slot(UD.75[[k]]@polygons[[1]], "area")/1000000,
                     "area.95" = slot(UD.95[[k]]@polygons[[1]], "area")/1000000)
        
  }
  
  tmp.95 <- lapply(UD.95, 
                   FUN = function(x) broom::tidy(spTransform(x, CRS("+proj=longlat"))))
  
  tmp.75 <- lapply(UD.75, 
                   FUN = function(x) broom::tidy(spTransform(x, CRS("+proj=longlat"))))
  tmp.50 <- lapply(UD.50, 
                   FUN = function(x) broom::tidy(spTransform(x, CRS("+proj=longlat"))))
  
  for (k in 1:length(tmp.95)){
    tmp.50[[k]] <- mutate(tmp.50[[k]], 
                          ID = kd.all$list.data$unique.ID[k])
    tmp.75[[k]] <- mutate(tmp.75[[k]], 
                          ID = kd.all$list.data$unique.ID[k])
    tmp.95[[k]] <- mutate(tmp.95[[k]], 
                          ID = kd.all$list.data$unique.ID[k])
  }
  
  df.95 <- do.call("rbind", tmp.95)
  df.75 <- do.call("rbind", tmp.75)
  df.50 <- do.call("rbind", tmp.50)
  
  out.list <- list(vert.95.df = df.95,
                   vert.75.df = df.75,
                   vert.50.df = df.50,
                   area = UD.area,
                   h = h.eachID,
                   h.multip = h.multip.eachID,
                   UD.95 = UD.95,
                   UD.75 = UD.75,
                   UD.50 = UD.50, 
                   UD.all = UD.all)
  return(out.list)
}


ID.names <- function(name){
  x <- unlist(strsplit(name, '_'))[1]
  return(x)
}

get.summary.1 <- function(dname, date.format = "%m-%d-%Y %H:%M:%S"){
  raw.files <- dir(path = dname, 
                   pattern = "_inout_DayNight.csv")
  
  GPS.files <- dir(path = dname, 
                   pattern = "_inside_DayNight_4hrs_GPS.csv")
  
  ARGOS.files <- dir(path = dname, 
                     pattern = "_inside_DayNight_4hrs_ARGOS.csv")
  
  raw.IDs <- unlist(lapply(raw.files, FUN = ID.names))
  
  col_def <- cols(ID = col_integer(),
                  ArgosID = col_integer(),
                  Message_Type = col_factor(levels = c("DIAG", "DS", "GPS")),
                  TransmissionDateTime = col_datetime(format = date.format),
                  UTCDateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  LC = col_integer(),
                  Residual = col_double(),
                  Lat1 = col_double(),
                  Lon1 = col_double(),
                  inside = col_integer(),
                  LocalDateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  Date2 = col_date(format = "%Y/%m/%d"),
                  LocalSunrise = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  LocalSunset =  col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  day1night2 = col_integer())
  
  raw <- do.call(rbind, 
                 lapply(raw.files,
                        FUN = function(x) readr::read_csv(file = paste0(dname, "/", x),
                                                          col_types = col_def))) %>%
    mutate(ID.f = as.factor(ArgosID)) %>%
    group_by(ID.f) %>%
    mutate(hr = hour(LocalDateTime)) %>%
    mutate(DaysSinceFirst = as.double((LocalDateTime - min(LocalDateTime))/(60*60*24)))
  
  GPS.IDs <- unlist(lapply(GPS.files, FUN = ID.names))
  ARGOS.IDs <- unlist(lapply(ARGOS.files, FUN = ID.names))
  
  col_def <- cols(ID = col_integer(),
                  ArgosID = col_integer(),
                  Message_Type = col_factor(levels = c("DIAG", "DS", "GPS")),
                  TransmissionDateTime = col_datetime(format = date.format),
                  UTCDateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  LC = col_integer(),
                  Residual = col_double(),
                  Lat1 = col_double(),
                  Lon1 = col_double(),
                  inside = col_integer(),
                  LocalDateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  Date2 = col_date(format = "%Y/%m/%d"),
                  LocalSunrise = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  LocalSunset =  col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  day1night2 = col_integer(),
                  hr = col_integer())
  
  GPS <- do.call(rbind, 
                 lapply(GPS.files,
                        FUN = function(x) readr::read_csv(file = paste0(dname, "/", x),
                                                          col_types = col_def))) %>%
    mutate(ID.f = as.factor(ArgosID)) %>%
    group_by(ID.f) %>%
    mutate(DaysSinceFirst = as.double((LocalDateTime - min(LocalDateTime))/(60*60*24)))
  
  ARGOS <- do.call(rbind, 
                   lapply(ARGOS.files,
                          FUN = function(x) readr::read_csv(file = paste0(dname, "/", x),
                                                            col_types = col_def))) %>%
    mutate(ID.f = as.factor(ArgosID)) %>%
    group_by(ID.f) %>%
    mutate(DaysSinceFirst = as.double((LocalDateTime - min(LocalDateTime))/(60*60*24)))
  
  raw %>%
    group_by(ArgosID) %>%
    mutate(day1 = ifelse(day1night2 == 1, 1, 0),
           night1 = ifelse(day1night2 == 2, 1, 0)) %>%
    summarise(Date1 = min(LocalDateTime), 
              n.days = max(DaysSinceFirst),
              n.relocations.all = n(),
              total.DIAG = sum(Message_Type == "DIAG"),
              total.DS = sum(Message_Type == "DS"),
              LC1 = sum(LC == 1, na.rm = T),
              LC2 = sum(LC == 2, na.rm = T),
              LC3 = sum(LC == 3, na.rm = T),
              ARGOS.outside = sum((Message_Type != "GPS" & inside == 0), na.rm = T),
              GPS = sum(Message_Type == "GPS", na.rm = T),
              GPS.reject = sum(Residual > res.limit, na.rm = T),
              GPS.outside = sum((Message_Type == "GPS" & inside == 0), na.rm = T),
              inside = sum(inside),
              n.day.all = sum(day1),
              n.night.all = sum(night1)) %>%
    arrange(Date1) %>%
    mutate(LC123 = LC1 + LC2 + LC3,
           ARGOS.accept = LC123 - ARGOS.outside,
           GPS.accept = GPS - GPS.reject - GPS.outside,
           n.data = ARGOS.accept + GPS.accept) -> raw.summary
  
  GPS %>%
    group_by(ArgosID) %>%
    mutate(day1 = ifelse(day1night2 == 1, 1, 0),
           night1 = ifelse(day1night2 == 2, 1, 0)) %>%
    summarise(Date1 = min(LocalDateTime), 
              n.days = max(DaysSinceFirst),
              n.relocations.all = n(),
              n.day.all = sum(day1),
              n.night.all = sum(night1)) %>%
    arrange(Date1) -> GPS.summary
  
  ID.min_n.GPS <- filter(GPS.summary,
                         n.relocations.all > (min_n - 1))
  
  ARGOS  %>%
    group_by(ArgosID) %>%
    mutate(day1 = ifelse(day1night2 == 1, 1, 0),
           night1 = ifelse(day1night2 == 2, 1, 0)) %>%
    summarise(Date1 = min(LocalDateTime), 
              n.days = max(DaysSinceFirst),
              n.relocations.all = n(),
              total.DIAG = sum(Message_Type == "DIAG"),
              total.DS = sum(Message_Type == "DS"),
              LC1 = sum(LC == 1, na.rm = T),
              LC2 = sum(LC == 2, na.rm = T),
              LC3 = sum(LC == 3, na.rm = T),
              n.day.all = sum(day1),
              n.night.all = sum(night1)) %>%
    arrange(Date1) -> ARGOS.summary
  
  ID.min_n.ARGOS <- filter(ARGOS.summary,
                           n.relocations.all > (min_n - 1))
  
  out.list <- list(ARGOS = ARGOS,
                   GPS = GPS,
                   raw = raw,
                   ARGOS.summary = ARGOS.summary,
                   GPS.summary = GPS.summary,
                   raw.summary = raw.summary,
                   ID.min_n.ARGOS = ID.min_n.ARGOS,
                   ID.min_n.GPS = ID.min_n.GPS)
  
  return(out.list)
}

#kd.input should be the output of HR.analysis.step1. 
run.HR.analysis <- function(kd.input, file.part, grid.value, h.multiplier, shape.file.dir){
  file.1 <- paste0("RData/h_adhoc_", file.part, ".rds")
  file.2 <- paste0("RData/h_", file.part, ".rds")
  file.3 <- paste0("RData/HR_", file.part, ".rds")
  
  if (!file.exists(file.1)){
    # this function uses only $all.utm so no individual HRs are computed.
    h.adhoc <- HR.analysis(h.multiplier, 
                           kd.input$kd.href, 
                           kd.input$list.data, 
                           grid = grid.value)
    saveRDS(h.adhoc,
            file = file.1)
  } else {
    h.adhoc <- readRDS(file = file.1)
  }
  
  if (!file.exists(file.2)){
    h.1 <- find.h.adhoc(kd.input$list.data$all.utm)
    saveRDS(h.1, 
            file = file.2)  
  } else {
    h.1 <- readRDS(file = file.2)
  }
  
  h <- round(h.1$h)
  
  if (!file.exists(file.3)){
    HR <- compute.area(h, 
                       kd.input$list.data$all.utm, 
                       grid = grid.value)
    saveRDS(HR, file = file.3)
    
  } else {
    HR <- readRDS(file.3)
  }
  
  writeOGR(obj = HR$ver.50, 
           dsn = paste0(shape.file.dir, file.part, "_HR_50.shp"),
           layer= paste0(file.part, ".50"), 
           driver="ESRI Shapefile", 
           overwrite_layer=TRUE)
  
  writeOGR(obj = HR$ver.95, 
           dsn = paste0(shape.file.dir, file.part,"_HR_95.shp"),
           layer="Pre.GPS.95", 
           driver="ESRI Shapefile", 
           overwrite_layer=TRUE)
  
  out.list <- list(h.adhoc = h.adhoc,
                   h.1 = h.1,
                   HR = HR)
  return(out.list)
}

extract.UD.eachID <- function(.kd, .data, data.name, grid.value){
  HR.byID <- data.frame(ID = .kd$list.data$unique.ID,
                        Area.50 = t(as.matrix(.data$HR$area.byID.50)),
                        Area.95 = t(as.matrix(.data$HR$area.byID.95))) 
  
  out.filename <- paste0("RData/HR_", data.name, "_eachID.rds")
  
  if (!file.exists(out.filename)){
    h.multip <- h <- vector(mode = "numeric", 
                            length = length(.kd$list.data$eachID.utm))
    
    UD.95 <- UD.50 <- vector(mode = "list", 
                             length = length(.kd$list.data$eachID.utm))
    for (k in 1:length(.kd$list.data$eachID.utm)){
      
      dat.utm <- .kd$list.data$eachID.utm[[k]]
      best.h <- find.h.adhoc(dat.utm)
      h[k] <- round(best.h$h)
      h.multip[k] <- best.h$h.multip
      UD <- kernelUD(dat.utm, h = h[k], 
                     kern = "bivnorm", grid = grid.value)
      UD.95[[k]] <- getverticeshr(UD, 95, unin = "m", unout = "km2")
      UD.50[[k]] <- getverticeshr(UD, 50, unin = "m", unout = "km2")
    }
    
    out.list <- list(UD.95 = UD.95,
                     UD.50 = UD.50,
                     h.multip = h.multip,
                     h = h)  
    
    saveRDS(out.list,
            file = out.filename)
  } else {
    
    out.list <- readRDS(out.filename)
  }
  
  return(out.list)  
}


#kd.input should be the output of HR.analysis.step1. No output file is saved.
run.HR.analysis.no.files <- function(kd.input, grid.value, h.multiplier){

  # this function uses only $all.utm so no individual HRs are computed.
  h.adhoc <- HR.analysis(h.multiplier, 
                         kd.input$kd.href, 
                         kd.input$list.data, 
                         grid = grid.value)

  h.1 <- find.h.adhoc(kd.input$list.data$all.utm)
    
  h <- round(h.1$h)
  
  HR <- compute.area(h, 
                     kd.input$list.data$all.utm, 
                     grid = grid.value)
    
  out.list <- list(h.adhoc = h.adhoc,
                   h.1 = h.1,
                   HR = HR)
  return(out.list)
}  

# extract area of intersection between HR (whatever that comes out of getverticeshr) and
# san diego bay = removes the land area. Input UD should be in longlat projection.
get.area <- function(UD){
  SDBay.geo <- spTransform(readOGR(dsn = "GISfiles",
                                   layer = "sd_bay",
                                   verbose = FALSE),
                           CRS("+proj=longlat +datum=WGS84"))
  
  if (is.list(UD)){
    areas <- lapply(UD, 
                    FUN = rgeos::gIntersection,
                    SDBay.geo)
  } else {
    areas <- rgeos::gIntersection(UD, SDBay.geo)
  }
  return(areas)  
}

# extract area of intersection between HR (whatever that comes out of getverticeshr) and
# san diego bay = removes the land area. Input UD should be in utm zone=11 projection.
get.area.UTM <- function(UD){
  SDBay.geo <- spTransform(readOGR(dsn = "GISfiles",
                                   layer = "sd_bay",
                                   verbose = FALSE),
                           CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  if (is.list(UD)){
    areas <- lapply(UD, 
                    FUN = rgeos::gIntersection,
                    SDBay.geo)
  } else {
    areas <- rgeos::gIntersection(UD, SDBay.geo)
  }
  return(areas)  
}


