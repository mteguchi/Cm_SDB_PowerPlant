#Deployment_table

rm(list=ls())

library(ggplot2)
library(viridis)
library(lubridate)
library(RODBC)
library(tidyverse)
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))

save.fig <- F
SWFSC <- T

if (SWFSC){
  turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')

  # get SD Bay results:
  turtle.SDB <- sqlQuery(turtle, 'select * from tbl_SD_Bay') %>%
    select(., NMFS_Tag, Turtle_ID, Year_caught,
           Month_caught, Day_caught, Caught_Dead,
           PIT_Tag_LFF, PIT_Tag_RFF, Inconel_Tag_LFF,
           Inconel_Tag_RFF, Sex, Weight_kg,
           Str_Carapace_Length_cm, Str_Carapace_Width_cm,
           Cur_Carapace_Length_cm, Cur_Carapace_Width_cm,
           Sat_Tag_Type, Sat_Tag_ID, Satellite_Tag_Added) %>%
    filter(., !is.na(NMFS_Tag)) %>%
    mutate(., Capture_Date = as.Date(paste0(Year_caught, '-',
                                            Month_caught, '-',
                                            Day_caught),
                                     format = '%Y-%m-%d'))

  odbcClose(turtle)
  write_rds(turtle.SDB, path = 'data/turtle_SDB.rds')
} else {
  turtle.SDB <- read_rds('data/turtle_SDB.rds')
}

dat.table <- read.csv('data/AdeHabitat_Summary_concise.csv')

# convert the date column into POSIX
dat.table$DateDeployed <- as.POSIXct(strptime(dat.table$DateDeployed,
                                               format = '%m/%d/%Y',
                                               tz = 'America/Los_Angeles'))
dat.table$Capture_Date <- as.Date(dat.table$DateDeployed)

dat.table$LastTransmissionDate<- as.POSIXct(strptime(dat.table$LastTransmissionDate,
                                                     format = '%m/%d/%Y',
                                                     tz = 'America/Los_Angeles'))

dat.table <- arrange(dat.table, DateDeployed)
dat.table$Yint <- seq(from = 1, to = nrow(dat.table), by = 1)/5

# merge with the other data table and get the mass. Need to pick oout
# the correct dates because some were caught multiple times:
turtle.SDB %>% select(., Turtle_ID, Weight_kg, Capture_Date) %>%
  right_join(., dat.table,
             by = c("Turtle_ID", "Capture_Date")) -> dat.table


# some testing:
lm.SCL <- lm(SCL ~ Period, data = dat.table)
lm.Days <- lm(TransmittingDays ~ Period, data = dat.table)


p1 <- ggplot(data = dat.table) +
  geom_segment(aes(x = DateDeployed,
                   xend = LastTransmissionDate,
                   y = Yint,
                   yend = Yint,
                   color = Sex),
               size = 2) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12, vjust = 0),
        legend.position = c(0.9, 0.2),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks.y = element_blank()) +
  scale_color_viridis(discrete = T,name = element_blank()) +
  geom_vline(xintercept = as.numeric(strptime('2010-12-31',
                                              format = '%Y-%m-%d')),
             size = 2, linetype = 4, color = 'darkgray') +
  xlab("Date") +
  ylab("") +
  geom_text(aes(x = DateDeployed+ days(floor(TransmittingDays/2)),
                y = Yint + 0.1,
                label = paste0(TransmittingDays, '(', Relocations, ')'),
                hjust = 'middle',
                vjust = 'center'))
  # geom_text(aes(x = DateDeployed,
  #               y = Yint + 0.1,
  #               label = TransmittingDays,
  #               vjust = 'center',
  #               hjust = 'middle'))


if (save.fig){
  ggsave(filename = 'figures/deployment_summary.png',
         dpi = 600,
         plot = p1,
         device = 'png',
         height = 4.53,
         width = 9.75,
         units = 'in')

}
