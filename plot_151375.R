#plot_151375

# plots sequence of analysis steps - removing erroneous data points

# created for making a slide for Urban Wildlife Conference in June 2017

rm(list=ls())

source('Cm_SDB_movements.R')
library(ggplot2)
library(StreamMetabolism)
library(viridis)

dpi <- 600
save.fig <- F

res.limit <- 35.0

dat.0 <- read.csv('data/151375_16Jun2016.csv')

dat.1 <- dat.0[, c('Message_Type', 'TransmissionDateTime', 'LC',
                   'Residual', 'Lat1', 'Lon1')]
dat.1$Date <- as.POSIXct(strptime(dat.0$TransmissionDateTime,
                                  format = "%m/%d/%y %H:%M",
                                  tz = 'GMT'))
dat.1$LocalDate <- as.POSIXct(format(dat.1$Date, format = '%Y-%m-%d %H:%M:%S',
                          tz = 'America/Los_Angeles'))

dat.1 <- dplyr::filter(dat.1, Message_Type == 'DS' | Message_Type == 'GPS')

#min.date <- as.character(min(dat.1$Date, na.rm = TRUE), format = "%Y-%m-%d")
#max.date <- as.character(max(dat.1$Date, na.rm = TRUE), format = "%Y-%m-%d")

base.map <- map.sdbay.zm

# plot all:
map.sdbay.zm.1 <- base.map +
  geom_point(data = dat.1,
             aes(x = Lon1, y = Lat1),
             color = "red",
             size = 1.0,
             alpha = 0.8) +

  xlab("") +
  ylab("") +
  ggtitle("All Locations") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))


# remove ARGOS A, B, Z, and 0
dat.2 <- dplyr::filter(dat.1, LC == 1 | LC == 2 | LC == 3 | Message_Type == "GPS")


map.sdbay.zm.2 <- base.map +
  geom_point(data = dat.2,
             aes(x = Lon1, y = Lat1),
             color = "red",
             size = 1.0,
             alpha = 0.8) +

  xlab("") +
  ylab("") +
  ggtitle("ARGOS filter") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))

# find inside-outside of the bay
in.out <- vector(mode = "numeric", length = dim(dat.2)[1])
for (k1 in 1:length(in.out)){
  in.out[k1] <- point.in.polygon(dat.2$Lon1[k1], dat.2$Lat1[k1],
                                 SDBay$long, SDBay$lat)
}
dat.2$inside <- in.out

dat.3 <- dplyr::filter(dat.2, inside == 1)
map.sdbay.zm.3 <- map.sdbay.south +
  geom_point(data = dat.3,
             aes(x = Lon1, y = Lat1),
             color = "red",
             size = 1.0,
             alpha = 0.8) +

  xlab("") +
  ylab("") +
  ggtitle("In/out") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))

# Split into 4-hr periods
file.out <- 'data/161375_4hr_tmp.csv'
if (file.exists(file.out)) file.remove(file.out)

dat.3$hr <- as.numeric(format(dat.3$LocalDate, '%H'))
dat.3$LocalDate2 <- format(dat.3$LocalDate, '%Y-%m-%d')
uniq.dates <- unique(dat.3$LocalDate2)
for (k in 1:length(uniq.dates)){
  tmp1 <- dat.3[dat.3$LocalDate2 == uniq.dates[k],]
  # then split into four-hr segments:
  # 0-4, 4-8, 8-12, 12-16, 16-20, 20-24
  for (k1 in 1:6){
    tmp2 <- tmp1[tmp1$hr >= ((k1-1) * 4) & tmp1$hr < (k1*4), ]
    if (dim(tmp2)[1] != 0){
      tmp2$block <- k1

      if (length(grep("GPS", tmp2$Message_Type)) > 1){
        tmp2.1 <- tmp2[grep("GPS", tmp2$Message_Type),]
        res.num <- as.numeric(tmp2.1$Residual)
        ifelse(res.num <= res.limit,
               tmp3 <- tmp2.1[res.num == min(res.num),],
               tmp3 <- NA)

      } else {
        LC.num <- as.numeric(tmp2$LC)
        tmp3 <- tmp2[LC.num == max(LC.num),]
      }

      if (length(tmp3) > 1){
        write.table(tmp3[1,], file = file.out, sep = ",",
                    append = T, quote = F, col.names = F,
                    row.names = F)

      }
    }
  }

}

dat.4 <- read.csv(file.out, header = F)
colnames(dat.4) <- c(colnames(dat.3), 'block')

dat.4$fblock <- as.factor(dat.4$block)

map.sdbay.zm.4 <- map.sdbay.south +
  geom_point(data = dat.4,
             aes(x = Lon1, y = Lat1, color = fblock),
             #color = "red",
             size = 1.5,
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
  ggtitle("4-hr block") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.1, 0.3))

if (save.fig){
  ggsave(filename = 'figures/151375_allPoints.png',
         dpi = dpi,
         plot = map.sdbay.zm.1,
         device = 'png')

  ggsave(filename = 'figures/151375_ArgosFilter.png',
         dpi = dpi,
         plot = map.sdbay.zm.2,
         device = 'png')

  ggsave(filename = 'figures/151375_InOut.png',
         dpi = dpi,
         plot = map.sdbay.zm.3,
         device = 'png')

  ggsave(filename = 'figures/151375_4hr.png',
         dpi = dpi,
         plot = map.sdbay.zm.4,
         device = 'png')
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
  ggtitle(paste0(ID, " (", min.date, ' : ', max.date, ")"))