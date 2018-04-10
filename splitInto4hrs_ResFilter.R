

rm(list=ls())

dname <- "data/"
all.files <- dir(dname, "_DayNight_")

# max residual value
res.limit <- 35.0

#filename <- "37616_DayNight_2016-06-08.csv"

for (k0 in 1:length(all.files)){
  id <- unlist(strsplit(all.files[k0], '_'))

  fileout <- paste0(dname, '/', id[1], "_DayNight_4hrs",
                    format(Sys.Date(), "_%Y-%m-%d"), ".csv")

  #fileout <- "37616_DayNight_4hrs.csv"
  #if (file.exists(fileout)) file.remove(fileout)

  dat <- read.csv(paste0(dname, '/', all.files[k0]))

  # create hours column:
  tmp <- strsplit(as.character(dat$Date), ' ')
  tmp2 <- lapply(tmp,
                 FUN = function(x) x[2])
  tmp3 <- lapply(tmp2,
                 FUN = function(x) unlist(strsplit(x, ':'))[1])

  dat$hr <- as.numeric(unlist(tmp3))

  write(x = colnames(dat), file = fileout,
        sep = ",", ncolumns = dim(dat)[2])

  # we need to know which dates we have
  # use Y/m/d column
  uniq.dates <- levels(dat$Date2)
  for (k in 1:length(uniq.dates)){
    tmp1 <- dat[dat$Date2 == uniq.dates[k],]
    # then split into four-hr segments:
    # 0-4, 4-8, 8-12, 12-16, 16-20, 20-24
    for (k1 in 1:6){
      tmp2 <- tmp1[tmp1$hr >= ((k1-1) * 4) & tmp1$hr < (k1*4), ]
      if (dim(tmp2)[1] != 0){
        if (length(grep("GPS", tmp2$LC)) > 1){
          tmp2.1 <- tmp2[grep("GPS", tmp2$LC),]
          res.num <- as.numeric(tmp2.1$Residual)
          ifelse(res.num <= res.limit,
                 tmp3 <- tmp2.1[res.num == min(res.num),],
                 tmp3 <- NA)

        } else {
          LC.num <- as.numeric(tmp2$LC)
          tmp3 <- tmp2[LC.num == max(LC.num),]
        }

        if (length(tmp3) > 1){
          write.table(tmp3[1,], file = fileout, sep = ",",
                      append = T, quote = F, col.names = F,
                      row.names = F)

        }
      }
    }

  }
}






