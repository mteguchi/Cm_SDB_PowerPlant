# creating maps for San Diego Bay
# some code were originally taken from presentation by Kim Gilbert, UBC
# modified since

# Tomo Eguchi
# 16 June 2016

rm(list=ls())
source('Cm_SDB_movements.R')
library(dplyr)

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

dpi <- 600
save.fig <- F

# for making plots for Urban Wildlife Conference in June 2017:
dir.name <- 'RawData/'
#dir.name <- 'data/'
data.files <- list.files(path = dir.name, pattern = '_DayNight_2016-06-30.csv')

locs.all <- vector(mode = 'list', length = length(data.files))
#names.all <- vector(mode = 'character', length= length(data.files))

k <- 1
for (k in 1:length(data.files)){
  dat.tmp <- read.csv(file = paste0(dir.name, data.files[k]))

  dat.tmp$Date3 <- as.POSIXct(strptime(dat.tmp$TransmissionDateTime,
                               "%m-%d-%Y %H:%M:%S", tz = 'GMT'))
  #min.date <- as.character(min(dat.tmp$Date3, na.rm = TRUE), format = "%Y-%m-%d")
  #max.date <- as.character(max(dat.tmp$Date3, na.rm = TRUE), format = "%Y-%m-%d")
    # let's try this with ggmap
  df1 <- na.omit(select(dat.tmp, lon = Lon1, lat = Lat1,
                        DateTime = Date3))

  df1$LocalDate <- as.POSIXct(format(df1$Date, format = '%Y-%m-%d %H:%M:%S',
                                       tz = 'America/Los_Angeles'))

  df1$hr <- as.numeric(format(df1$LocalDate, '%H'))

  # find inside-outside of the bay
  in.out <- vector(mode = "numeric", length = dim(df1)[1])
  for (k1 in 1:length(in.out)){
    in.out[k1] <- point.in.polygon(df1$lon[k1], df1$lat[k1],
                                   SDBay$long, SDBay$lat)
  }

  df1$inside <- in.out
  df1 <- df1[df1$inside == 1,]
  df1$blocks <- NA
  for (k1 in 1:nrow(df1)){
    if (df1$hr[k1] >= 0 & df1$hr[k1] < 4) {
      df1$blocks[k1] <- 1
    } else if (df1$hr[k1] >= 4 & df1$hr[k1] < 8) {
      df1$blocks[k1] <- 2
    } else if (df1$hr[k1] >= 8 & df1$hr[k1] < 12) {
      df1$blocks[k1] <- 3
    } else if (df1$hr[k1] >= 12 & df1$hr[k1] < 16){
      df1$blocks[k1] <- 4
    } else if (df1$hr[k1] >= 16 & df1$hr[k1] < 20){
      df1$blocks[k1] <- 5
    } else if (df1$hr[k1] >= 20) {
      df1$blocks[k1] <- 6
    }
  }

  ID <- unlist(strsplit(data.files[k],
                        '_DayNight_2016-06-30.csv'))[1]

  df1$ID <- ID
  locs.all[[k]] <- df1
  #names.all[k] <- ID

}

all.data <- do.call('rbind', locs.all )

all.data$fblock <- as.factor(all.data$blocks)
all.data$Yr <- as.numeric(format(all.data$LocalDate, '%Y'))

all.data.pre <- filter(all.data, Yr < 2010)
all.data.post <- filter(all.data, Yr >= 2010)

base.map <- map.sdbay.med
plot.pre <- base.map +
    geom_point(data = all.data.pre,
               aes(x = lon,
                   y = lat,
                   color = fblock),
               size = 1.0,
               alpha = 0.8) +
  scale_color_viridis(discrete = T,
                      labels = c('0000-0400',
                                 '0400-0800',
                                 '0800-1200',
                                 '1200-1600',
                                 '1600-2000',
                                 '2000-2400'),
                      name = element_blank()) +

  xlab("") +
  ylab("") +
  #ggtitle("4-hr block") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.1, 0.3)) +
  geom_polygon(data = SDBay.eelg.2008.df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "green",
               alpha = 0.2)

plot.post <- base.map +
  geom_point(data = all.data.post,
             aes(x = lon,
                 y = lat,
                 color = fblock),
             size = 1.0,
             alpha = 0.8) +
  scale_color_viridis(discrete = T,
                      labels = c('0000-0400',
                                 '0400-0800',
                                 '0800-1200',
                                 '1200-1600',
                                 '1600-2000',
                                 '2000-2400'),
                      name = element_blank()) +

  xlab("") +
  ylab("") +
  #ggtitle("4-hr block") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.1, 0.3)) +
  geom_polygon(data = SDBay.eelg.2014.df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "green",
               alpha = 0.2)

#map.sdbay.zm.eelg14
if (save.fig){
  ggsave(filename = 'figures/preClosureDistribution.png',
         plot = plot.pre,
         dpi = dpi,
         device = 'png')
  ggsave(filename = 'figures/postClosureDistribution.png',
         plot = plot.post,
         dpi = dpi,
         device = 'png')

}




