#HR_analysis_functions

library(dplyr)
library(adehabitatHR)
library(rgdal)
library(leaflet)

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

  coordinates(all.coords) <- ~ x + y
  proj4string(all.coords) <- CRS(latlong)
  all.utm <- spTransform(all.coords, tagproj)

  byID.coords <- data.frame(x=selected$Lon1,
                            y=selected$Lat1,
                            ID = as.factor(selected$ArgosID))

  coordinates(byID.coords) <- ~ x + y
  proj4string(byID.coords) <- CRS(latlong)
  byID.utm <- spTransform(byID.coords, tagproj)

  eachID.utm <- eachID.coords <- list()

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
  }

  return(list(all.coords = all.coords,
              all.utm = all.utm,
              byID.coords = byID.coords,
              byID.utm = byID.utm,
              eachID.coords = eachID.coords,
              eachID.utm = eachID.utm,
              unique.ID = unique.ID))
}

# A function to compute UDs using all data and for each individual. Returns a list
# of estimated bandwidth, kernel density estimates as well as areas
# for 50 and 95% UDs
HR.bvnorm.fcn <- function(all.utm,
                          byID.utm,
                          h="href",
                          hlim=c(0.03, 1.5),
                          grid=300, extent = 1){

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

  byID.kd <- kernelUD(byID.utm,
                      h = all.kd@h$h ,
                      hlim = hlim,
                      grid = grid,
                      extent = extent,
                      kern = "bivnorm")

  Area.byID <- kernel.area(byID.kd,
                           percent = c(50, 95),
                           unin = c("m"),
                           unout = c("km2"),
                           standardize = FALSE)

  bw <- all.kd@h$h ##bandwidth estimate
  #Area.50 <- Area.all["50"]
  #Area.95 <- Area.all["95"]

  #ver.50 <- getverticeshr(all.kd, 50)
  #ver.95 <- getverticeshr(all.kd, 95)

  return(list(bw = bw,
              all.kd = all.kd,
              byID.kd = byID.kd,
              area.all = Area.all,
              area.byID = Area.byID))
}

# A function to conduct the first step of UD analysis. It takes a full dataset
# and select individuals with at least min_n (50) data points. Computes UD using the
# reference bandwidth (href) and the method of least square cross validation (LSCV).
# UD from the LSCV method can be plotted to find the bandwidth that minimizes the
# CV of bandwidth.
HR.analysis.step1 <- function(min_n = 50, data.df, tagproj, grid=300){
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
                      hlim = c(0.03, 1.5),
                      grid=300,
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
HR.analysis <- function(h.multiplier, kd.href, data.list, grid = 300){
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
                         percent = 95)

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
                       limits = c(-117.15, -117.09))+
    scale_y_continuous(breaks = c(32.62, 32.64, 32.66, 32.68),
                       limits = c(32.60, 32.70))

  return(list(figure = p.h.adhoc,
              ver.95 = ver.95.adhoc.df,
              h = h.adhoc))
}

# A function to compute 50 and 95% UD areas. INputs are bandwidth (h),
# the output from make.HR.dataset, and grid value. It uses HR.bvnorm.fcn.
# Returns 50 and 95% areas and vertices, which are used to plot UDs.
compute.area <- function(h, list.data, grid=300){
  HR <- HR.bvnorm.fcn(list.data$all.utm,
                      list.data$byID.utm,
                      h = h,
                      hlim=c(0.03, 1.5),
                      grid=grid, extent = 1)
  bw <- HR$all.kd@h$h ##bandwidth estimate

  return(list(area.50 = HR$area.all["50"],
              area.95 = HR$area.all["95"],
              ver.50 = getverticeshr(HR$all.kd, 50),
              ver.95 = getverticeshr(HR$all.kd, 95),
              area.byID.50 = HR$area.byID["50",],
              area.byID.95 = HR$area.byID["95",],
              ver.byID.50 = getverticeshr(HR$byID.kd, 50),
              ver.byID.95 = getverticeshr(HR$byID.kd, 95)))
}

# This funciton finds a bandwidth value that makes 95% UD contiguous
find.h.adhoc <- function(utm.data, grid = 300, extent = 1){
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
    tmp.V <- getverticeshr(tmp.UD, 95)
    n.seg <- length(tmp.V@polygons[[1]]@Polygons)
    h.multip <- h.multip + 0.05
  }
  return(list(v.95 = tmp.V,
              UD.95 = tmp.UD,
              h = h,
              href = h1,
              h.multip = h.multip))
}
