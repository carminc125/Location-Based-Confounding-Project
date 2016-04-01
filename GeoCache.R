setwd("/Users/carmincovarrubias/Dropbox/R-Script/")
install.packages("R.cache")
library(R.cache)


#reads csv file of new data
d <- read.csv(file = "Addresses.csv", sep = ",", stringsAsFactors = FALSE)

#translates addreseses to lat/longs and slows geocoding with Sys.sleep
slow <- function(d) {
  latlong <- data.frame()
  for(i in 1:nrow(d)) {
    latlong <- rbind(latlong, geocode(d$Addresses[i]))
    Sys.sleep(.5)
  }
  return(latlong)
}

#saves lat/longs in an object
writeLats<- slow(d)
write.csv(writeLats, file = "latlong.csv", row.names = FALSE)
read.csv("latlong.csv")
