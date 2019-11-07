#Deployment_table

rm(list=ls())

library(ggplot2)
library(viridis)
library(lubridate)
library(RODBC)
library(tidyverse)
library(dplyr)

save.fig <- F
SWFSC <- T

if (SWFSC){
  turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')

  # get SD Bay results:
  turtle.SDB <- sqlQuery(turtle, 'select * from tbl_SD_Bay') %>% 
    dplyr::select(NMFS_Tag, Turtle_ID, Year_caught,
           Month_caught, Day_caught, Caught_Dead,
           PIT_Tag_LFF, PIT_Tag_RFF, Inconel_Tag_LFF,
           Inconel_Tag_RFF, Sex, Weight_kg,
           Str_Carapace_Length_cm, Str_Carapace_Width_cm,
           Cur_Carapace_Length_cm, Cur_Carapace_Width_cm,
           Sat_Tag_ID, Satellite_Tag_Added) %>%
    filter(., !is.na(NMFS_Tag)) %>%
    filter(., !is.na(Sat_Tag_ID)) %>%
    mutate(., Capture_Date = as.Date(paste0(Year_caught, '-',
                                            Month_caught, '-',
                                            Day_caught),
                                     format = '%Y-%m-%d'))

  odbcClose(turtle)
  readr::write_csv(turtle.SDB, path = 'data/turtle_SDB.csv')
} else {
  turtle.SDB <- read.csv('data/turtle_SDB.csv')
}

file.date <- "2019-11-01"
# for 2018-10-31 file, ID.f has been changed to ArgosID... 
if (file.date != "2018-10-31"){
  col.def <- cols(ID.f = col_integer(),
                  Date1 = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  n.days = col_double(),
                  n.relocations.all = col_integer(),
                  n.day.all = col_integer(),
                  n.night.all = col_integer(),
                  n.day = col_integer(),
                  n.night = col_integer(),
                  n.relocations = col_integer())
  
  dat.table.pre <- readr::read_csv(paste0("data/pre_sample_summary_",
                                          file.date, ".csv"),
                                   col_types = col.def)
  
} else {
  col.def <- cols(ArgosID = col_integer(),
                  Date1 = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                  n.days = col_double(),
                  n.relocations.all = col_integer(),
                  n.day.all = col_integer(),
                  n.night.all = col_integer(),
                  n.day = col_integer(),
                  n.night = col_integer(),
                  n.relocations = col_integer())
  
  dat.table.pre <- readr::read_csv(paste0("data/pre_sample_summary_",
                                          file.date, ".csv"),
                                   col_types = col.def)
  
  dat.table.pre %>% mutate(ID.f = as.factor(ArgosID)) -> dat.table.pre
  
}

dat.table.pre %>% mutate(date = as.Date(Date1, 
                                        format = "%Y-%m-%d")) -> dat.table.pre

# try to merge turtle.SDB and dat.table.pre:
dat.list <- list()
#k <- 7
for (k in 1:nrow(dat.table.pre)){
  tmp.turtle.SDB <- filter(turtle.SDB, 
                           Sat_Tag_ID == dat.table.pre$ID.f[k])
  if (nrow(tmp.turtle.SDB) == 1){
    tmp.turtle.SDB %>% transmute(ID.f = dat.table.pre$ID.f[k],
                                 Turtle_ID = Turtle_ID,
                                 Capture_Date = as.Date(Capture_Date,
                                                        format = "%Y-%m-%d"),
                                 Sex = Sex,
                                 Mass_kg = Weight_kg,
                                 SCL_cm = Str_Carapace_Length_cm,
                                 SCW_cm = Str_Carapace_Width_cm,
                                 CCL_cm = Cur_Carapace_Length_cm,
                                 CCW_cm = Cur_Carapace_Width_cm) -> dat.list[[k]]
  } else {
    tmp.turtle.SDB %>% filter(Year_caught == year(dat.table.pre$Date1[k])) -> tmp.turtle.SDB
    if (nrow(tmp.turtle.SDB == 1)){
      tmp.turtle.SDB %>% transmute(ID.f = dat.table.pre$ID.f[k],
                                   Turtle_ID = Turtle_ID,
                                   Capture_Date = as.Date(Capture_Date,
                                                          format = "%Y-%m-%d"),
                                   Sex = Sex,
                                   Mass_kg = Weight_kg,
                                   SCL_cm = Str_Carapace_Length_cm,
                                   SCW_cm = Str_Carapace_Width_cm,
                                   CCL_cm = Cur_Carapace_Length_cm,
                                   CCW_cm = Cur_Carapace_Width_cm) -> dat.list[[k]]
    }
  }
}

dat.pre.morph <- do.call("rbind", dat.list)

dat.table.pre <- full_join(dat.table.pre, dat.pre.morph, by = "ID.f")%>%
  mutate(period = "pre")

if (!file.exists(paste0("data/pre_sample_summary_size_",
                        file.date, ".csv")))
  write.csv(dat.table.pre,
            paste0("data/pre_sample_summary_size_",
                   file.date, ".csv"),
            quote = F,
            row.names = F)

#Do the post period also:
dat.table.post <- readr::read_csv(paste0("data/post_sample_summary_",
                                         file.date, ".csv"),
                                  col_types = col.def)

if (file.date == "2018-10-31"){
  dat.table.post %>% mutate(ID.f = as.factor(ArgosID)) -> dat.table.post
}

dat.table.post %>% mutate(date = as.Date(Date1, format = "%Y-%m-%d")) -> dat.table.post

# try to merge turtle.SDB and dat.table.pre:
dat.list <- list()
#k <- 1
for (k in 1:nrow(dat.table.post)){
  tmp.turtle.SDB <- filter(turtle.SDB, Sat_Tag_ID == dat.table.post$ID.f[k])
  if (nrow(tmp.turtle.SDB) == 1){
    tmp.turtle.SDB %>% transmute(ID.f = dat.table.post$ID.f[k],
                                 Turtle_ID = Turtle_ID,
                                 Capture_Date = as.Date(Capture_Date,
                                                        format = "%Y-%m-%d"),
                                 Sex = Sex,
                                 Mass_kg = Weight_kg,
                                 SCL_cm = Str_Carapace_Length_cm,
                                 SCW_cm = Str_Carapace_Width_cm,
                                 CCL_cm = Cur_Carapace_Length_cm,
                                 CCW_cm = Cur_Carapace_Width_cm) -> dat.list[[k]]
  } else if (nrow(tmp.turtle.SDB) > 1){
    tmp.turtle.SDB %>%
      filter(Year_caught == year(dat.table.post$Date1[k])) -> tmp.turtle.SDB
    if (nrow(tmp.turtle.SDB == 1)){
      tmp.turtle.SDB %>% transmute(ID.f = dat.table.post$ID.f[k],
                                   Turtle_ID = Turtle_ID,
                                   Capture_Date = as.Date(Capture_Date,
                                                          format = "%Y-%m-%d"),
                                   Sex = Sex,
                                   Mass_kg = Weight_kg,
                                   SCL_cm = Str_Carapace_Length_cm,
                                   SCW_cm = Str_Carapace_Width_cm,
                                   CCL_cm = Cur_Carapace_Length_cm,
                                   CCW_cm = Cur_Carapace_Width_cm) -> dat.list[[k]]
    }
  } else if (nrow(tmp.turtle.SDB) == 0){  # this is when a same Argos ID was used > 1
    newID <- floor(dat.table.post$ID.f[k]/100)
    tmp.turtle.SDB <- filter(turtle.SDB, Sat_Tag_ID == newID &
                               Year_caught == year(dat.table.post$Date1[k]) &
                               Month_caught == month(dat.table.post$Date1[k]))

    tmp.turtle.SDB %>% transmute(ID.f = dat.table.post$ID.f[k],
                                 Turtle_ID = Turtle_ID,
                                 Capture_Date = as.Date(Capture_Date,
                                                        format = "%Y-%m-%d"),
                                 Sex = Sex,
                                 Mass_kg = Weight_kg,
                                 SCL_cm = Str_Carapace_Length_cm,
                                 SCW_cm = Str_Carapace_Width_cm,
                                 CCL_cm = Cur_Carapace_Length_cm,
                                 CCW_cm = Cur_Carapace_Width_cm) -> dat.list[[k]]
  }
}

dat.post.morph <- do.call("rbind", dat.list)

dat.table.post <- full_join(dat.table.post, dat.post.morph, by = "ID.f") %>%
  mutate(period = "post")

if (!file.exists(paste0("data/post_sample_summary_size_",
                        file.date, ".csv")))
  write.csv(dat.table.post,
            paste0("data/post_sample_summary_size_",
                   file.date, ".csv"),
            quote = F,
            row.names = F)

dat.table <- rbind(dat.table.pre, dat.table.post)
# convert the date column into POSIX
# dat.table$DateDeployed <- as.POSIXct(strptime(dat.table$Capture_Date,
#                                                format = '%m/%d/%Y',
#                                                tz = 'America/Los_Angeles'))
# dat.table$Capture_Date <- as.Date(dat.table$DateDeployed)
#
# dat.table$LastTransmissionDate<- as.POSIXct(strptime(dat.table$LastTransmissionDate,
#                                                      format = '%m/%d/%Y',
#                                                      tz = 'America/Los_Angeles'))

dat.table <- arrange(dat.table, Date1)
dat.table$Yint <- seq(from = 1, to = nrow(dat.table), by = 1)/5

if (!file.exists(paste0("data/all_sample_summary_size_",
                        file.date, ".csv")))
  write.csv(dat.table,
            paste0("data/all_sample_summary_size_",
                   file.date, ".csv"),
            quote = F,
            row.names = F)

# some testing:
lm.SCL <- lm(SCL_cm ~ period, data = dat.table)
lm.Days <- lm(n.days ~ period, data = dat.table)

dat.table %>% mutate(end.date = Date1 + n.days) -> dat.table

# create a plot to show how # transmissions was reduced



# not very useful plot...
# p1 <- ggplot(data = dat.table) +
#   geom_segment(aes(x = Date1,
#                    xend = Date1 + n.days*24*60*60,
#                    y = Yint,
#                    yend = Yint,
#                    color = Sex),
#                size = 2) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 12, hjust = 0.5),
#         legend.text = element_text(size = 12, vjust = 0),
#         legend.position = c(0.9, 0.2),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 12),
#         axis.ticks.y = element_blank()) +
#   scale_color_viridis(discrete = T,name = element_blank()) +
#   geom_vline(xintercept = as.numeric(strptime('2010-12-31',
#                                               format = '%Y-%m-%d')),
#              size = 2, linetype = 4, color = 'darkgray') +
#   xlab("Date") +
#   ylab("")
#   geom_text(aes(x = DateDeployed+ days(floor(TransmittingDays/2)),
#                 y = Yint + 0.1,
#                 label = paste0(TransmittingDays, '(', Relocations, ')'),
#                 hjust = 'middle',
#                 vjust = 'center'))
#   # geom_text(aes(x = DateDeployed,
#   #               y = Yint + 0.1,
#   #               label = TransmittingDays,
#   #               vjust = 'center',
#   #               hjust = 'middle'))
#
#
# if (save.fig){
#   ggsave(filename = 'figures/deployment_summary.png',
#          dpi = 600,
#          plot = p1,
#          device = 'png',
#          height = 4.53,
#          width = 9.75,
#          units = 'in')
#
# }
