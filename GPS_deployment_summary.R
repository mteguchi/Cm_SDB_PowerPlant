#captureEffort


# extracts capture effort data from the database

rm(list=ls())
library(RODBC)
library(tidyverse)
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/tools/TomosFunctions.R'),
       source('~/R/tools/TomosFunctions.R'))
SWFSC <- T

if (SWFSC){
  turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
  turtle.tbls <- sqlTables(turtle)

  # TO FIND ALL TABLE NAMES:
  # turtle.tbls.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'tbl_',
  #                                                  turtle.tbls$TABLE_NAME)]
  #
  # turtle.vw.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'vw_',
  #                                                turtle.tbls$TABLE_NAME)]

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
    mutate(., Capture_Date = as.POSIXct(strptime(paste0(Year_caught, '-',
                                                        Month_caught, '-',
                                                        Day_caught),
                                                 format = '%Y-%m-%d',
                                                 tz = 'America/Los_Angeles')))

  odbcClose(turtle)
  write_rds(turtle.SDB, path = 'data/turtle_SDB.rds')
} else {
  turtle.SDB <- read_rds('data/turtle_SDB.rds')
}

# number of days with turtles:
n.days <- group_by(turtle.SDB, Capture_Date) %>%
  summarise(., n = n())

seasons <- group_by(turtle.SDB, Year_caught) %>%
  summarise(., min_month = min(Month_caught),
            max.month = max(Month_caught))

sat.tags <- filter(turtle.SDB, Satellite_Tag_Added == 1) %>%
  filter(., Capture_Date >= '2007-01-01' & Capture_Date <= '2014-12-31')

dat.table <- read.csv('data/AdeHabitat_Summary_concise.csv')

# convert the date column into POSIX
dat.table$DateDeployed <- as.POSIXct(strptime(dat.table$DateDeployed,
                                              format = '%m/%d/%Y',
                                              tz = 'America/Los_Angeles'))

dat.table$LastTransmissionDate<- as.POSIXct(strptime(dat.table$LastTransmissionDate,
                                                     format = '%m/%d/%Y',
                                                     tz = 'America/Los_Angeles'))


left_join(sat.tags, dat.table, by = c('Turtle_ID', 'Capture_Date' = 'DateDeployed')) %>%
  select(., Turtle_ID, Sex.x, Sex.y, Weight_kg, Str_Carapace_Length_cm,
         SCL, Str_Carapace_Width_cm, Cur_Carapace_Length_cm,
         Cur_Carapace_Width_cm, Sat_Tag_ID, Capture_Date, Period,
         LastTransmissionDate,
         TransmittingDays, Relocations) -> sat.tags

# fill in NAs in various fields by looking up the entire dataset and finding the closest
# time:
weight.na <- filter(sat.tags, is.na(Weight_kg))
k <- 1
for (k in 1:nrow(weight.na)){
  tmp <- filter(turtle.SDB, Turtle_ID == weight.na[k, 'Turtle_ID']) %>%
    filter(., !is.na(Weight_kg))
  dt <- abs(tmp$Capture_Date - weight.na[k, 'Capture_Date'])
  tmp.1 <- tmp[order(dt) == 1,]
  sat.tags[sat.tags$Turtle_ID == weight.na[k, 'Turtle_ID'], 'Weight_kg'] <- tmp.1$Weight_kg
}

mean.SCL <- mean(sat.tags$SCL, na.rm = T)
SE.SCL <- SE(sat.tags$SCL)
min.SCL <- min(sat.tags$SCL, na.rm = T)
max.SCL <- max(sat.tags$SCL, na.rm = T)

mean.pre.SCL <- mean(filter(sat.tags, Period == 'Pre')$SCL, na.rm = T)
SE.pre.SCL <- SE(filter(sat.tags, Period == 'Pre')$SCL)

mean.post.SCL <- mean(filter(sat.tags, Period == 'Post')$SCL, na.rm = T)
SE.post.SCL <- SE(filter(sat.tags, Period == 'Post')$SCL)

mean.mass <- mean(sat.tags$Weight_kg, na.rm = T)
SE.mass <- SE(sat.tags$Weight_kg)
min.mass <- min(sat.tags$Weight_kg, na.rm = T)
max.mass <- max(sat.tags$Weight_kg, na.rm = T)

mean.pre.mass <- mean(filter(sat.tags, Period == 'Pre')$Weight_kg, na.rm = T)
SE.pre.mass <- SE(filter(sat.tags, Period == 'Pre')$Weight_kg)

mean.post.mass <- mean(filter(sat.tags, Period == 'Post')$Weight_kg, na.rm = T)
SE.post.mass <- SE(filter(sat.tags, Period == 'Post')$Weight_kg)

mean.days <- mean(sat.tags$TransmittingDays)
SE.days <- SE(sat.tags$TransmittingDays)
min.days <- min(sat.tags$TransmittingDays)
max.days <- max(sat.tags$TransmittingDays)

mean.pre.days <- mean(filter(sat.tags, Period == 'Pre')$TransmittingDays, na.rm = T)
SE.pre.days <- SE(filter(sat.tags, Period == 'Pre')$TransmittingDays)

mean.post.days <- mean(filter(sat.tags, Period == 'Post')$TransmittingDays, na.rm = T)
SE.post.days <- SE(filter(sat.tags, Period == 'Post')$TransmittingDays)

sat.tags.out <- select(sat.tags, Period, SCL, Weight_kg, Sex.y,
                       Capture_Date, LastTransmissionDate, TransmittingDays,
                       Relocations)

#write_csv(sat.tags.out, path = 'data/deploymentSummary.csv')

# compare body sizes between pre and post
fit.mass <- lm(Weight_kg ~ Period, data = sat.tags)

fit.SCL <- lm(SCL ~ Period, data = sat.tags)

fit.days <- lm(TransmittingDays ~ Period, data = sat.tags)

# SEs computed above are different from what
# are provided in linear model fits - linear models use the following:
# from here: https://stats.stackexchange.com/questions/44838/how-are-the-standard-errors-of-coefficients-calculated-in-a-regression
# one variance is assumed so SEhats are smaller than what are computed
# for SEs when they are independently calculated.
X <- as.matrix(cbind(c(rep(1, 11), rep(0, 7)),
                     c(rep(0, 11), rep(1, 7))))
y <- c(filter(sat.tags, Period == 'Post')$Weight_kg,
       filter(sat.tags, Period == 'Pre')$Weight_kg)

vBeta <- solve(t(X)%*%X, t(X)%*%y)
dSigmaSq <- sum((y - X%*%vBeta)^2)/(nrow(X)-ncol(X))
mVarCovar <- dSigmaSq*chol2inv(chol(t(X)%*%X))
SEhats <- sqrt(diag(mVarCovar))

# Use effort data from Katie's version - data have been checked with
# datasheets - added Na1 and NA2 as two missing headings on 11/13/2017
# col_def.effort <- cols(ID = col_integer(),
#                       Field_Date = col_date(format = '%m/%d/%Y'),
#                       Net_Color = col_character(),
#                       Net_Deployment_Time = col_time(format = '%H:%M:%S'),
#                       Net_Retrieval_Time = col_time(format = '%H:%M:%S'),
#                       Net_Deployment_Location = col_character(),
#                       GPS_N_Center_Nets = col_double(),
#                       GPS_W_Center_Nets = col_double(),
#                       Net_Lat_End1 = col_double(),
#                       Net_Lon_End1 = col_double(),
#                       Net_Lat_End2 = col_double(),
#                       Net_Lon_End2 = col_double(),
#                       High_Tide = col_character(),
#                       Low_Tide = col_character(),
#                       Net_Location_Notes = col_character(),
#                       NA1 = col_character(),
#                       NA2 = col_character(),
#                       Comment = col_character())
#
# turtle.effort <- read_csv(paste0(dirSelector()$Rdir,
#                                  '/Cm_SDB_tides/data/Standardized Net data.csv'),
#                           col_types = col_def.effort) %>%
#   select(., Field_Date, Net_Deployment_Time, Net_Retrieval_Time,
#          Net_Deployment_Location, GPS_N_Center_Nets, GPS_W_Center_Nets,
#          Net_Lat_End1, Net_Lon_End1, Net_Lat_End2, Net_Lon_End2)

# to see all columns using head, do head(data.frame(turtle.effort))


