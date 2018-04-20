# Function version of determining whether or not each data point is inside or
# outside of SDB

# determine if locations are in the bay

inside_outside <- function(data.file.name){
  dat <- read.csv(data.file.name)
  in.out <- vector(mode = "numeric", length = dim(dat)[1])
  for (k1 in 1:length(in.out)){
    in.out[k1] <- point.in.polygon(dat$Lon1[k1], dat$Lat1[k1],
                                   SDBay$long, SDBay$lat)
  }
  dat$inside <- in.out
  return(dat)

}


