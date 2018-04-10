#SD_Bay_Satellite_Report




rm(list=ls())
library(RODBC)
library(dplyr)

# define if ODBC is available
odbc <- T
# then set when it was run:
Run.Date <- '2017-07-17'

if (odbc){
  # load a couple databases through ODBC
  turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
  #LIMS <- odbcConnect(dsn = 'LIMS', uid = '', pwd = '')

  turtle.tbls <- sqlTables(turtle)
  #LIMS.tbls <- sqlTables(LIMS)

  # TO FIND ALL TABLE NAMES:
  turtle.tbls.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'tbl_',
                                                   turtle.tbls$TABLE_NAME)]

  turtle.vw.names <- turtle.tbls$TABLE_NAME[grep(pattern = 'vw_',
                                                 turtle.tbls$TABLE_NAME)]

  # LIMS.tbls.names <- LIMS.tbls$TABLE_NAME[grep(pattern = 'tbl_',
  #                                              LIMS.tbls$TABLE_NAME)]
  #
  # LIMS.vw.names <- LIMS.tbls$TABLE_NAME[grep(pattern = 'vw_',
  #                                            LIMS.tbls$TABLE_NAME)]

  # get SD Bay results:
  turtle.SDB <- sqlQuery(turtle, 'select * from tbl_SD_Bay')
  turtle.SDB <- turtle.SDB[, c('NMFS_Tag', 'Turtle_ID', 'Year_caught',
                               'Month_caught', 'Day_caught', 'Caught_Dead',
                               'PIT_Tag_LFF', 'PIT_Tag_RFF', 'Inconel_Tag_LFF',
                               'Inconel_Tag_RFF', 'Sex', 'Weight_kg',
                               'Str_Carapace_Length_cm', 'Str_Carapace_Width_cm',
                               'Cur_Carapace_Length_cm', 'Cur_Carapace_Width_cm',
                               'Sat_Tag_Type', 'Sat_Tag_ID')]

  turtle.SDB <- turtle.SDB[!is.na(turtle.SDB$NMFS_Tag),]
  # haplotypes
  #haplos.turtles <- sqlQuery(LIMS, 'select * from vw_Sequence_Latest_Run')

  # turtle archive:
  #turtle.archive.tbl <- sqlQuery(turtle, 'select * from tbl_Turtle_Archive')

  odbcClose(turtle)
  #odbcClose(LIMS)

  turtle.SDB.sat <- turtle.SDB[!is.na(turtle.SDB$Sat_Tag_ID),]

  turtle.SDB.sat$Date <- as.Date(paste(turtle.SDB.sat$Year_caught,
                                       turtle.SDB.sat$Month_caught,
                                       turtle.SDB.sat$Day_caught, sep = '-'),
                                 format = '%Y-%m-%d')

  save(turtle.SDB.sat,
       file = paste0('RData/turtle_SDB_satellite_', Sys.Date(), '.RData'))
} else {

  turtle.SDB.sat <- load('RData/turtle_SDB_satellite_', Run.Date, '.RData')
}


turtle.SDB.sat.2017 <- dplyr::filter(turtle.SDB.sat,
                                     Date >= as.Date('2016-05-01') &
                                       Date <= as.Date('2017-04-30'))
