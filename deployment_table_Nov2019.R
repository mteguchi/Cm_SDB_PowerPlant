#Deployment_table

## !!!  DO NOT SOURCE THIS SCRIPT!!  ###
# MAY NEED TO MANUALLY MODIFY AN INPUT FILE #

rm(list=ls())

library(ggplot2)
library(viridis)
library(lubridate)
library(RODBC)
library(tidyverse)
library(dplyr)

save.fig <- F
SWFSC <- T

if (SWFSC & !file.exists("data/turtle_SDB_corrected.csv")){
  turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')

  # get SD Bay results:
  turtle.SDB <- sqlQuery(turtle, 'select * from tbl_SD_Bay') %>% 
    dplyr::select(Turtle_ID, Year_caught,
           Month_caught, Day_caught, Sex, Weight_kg,
           Str_Carapace_Length_cm, Str_Carapace_Width_cm,
           Cur_Carapace_Length_cm, Cur_Carapace_Width_cm,
           Sat_Tag_ID) %>%
    filter(!is.na(Sat_Tag_ID))

  odbcClose(turtle)
  readr::write_csv(turtle.SDB, path = 'data/turtle_SDB.csv')
}

# need to fix Sat_Tag_ID for four turtles in turtle_SDB manually.
#126069 -> 12606905 5/13/2014
#126071 -> 12607106 6/26/2014
#126069 -> 12606907 7/24/2014
#126071 -> 12607107 7/24/2014
#21136/13183 -> 13183 This seems incorrect but it will be filtered out for now. 
# need to fill in missing CCL also: 52675 on 3/7/2007 and 44366 on 3/25/2009


