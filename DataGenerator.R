setwd("/Users/carmincovarrubias/Dropbox/R-Script/")
rm(list=ls())
source("Partly_Linear_Feb5.R")
#install.packages("fields")
library(fields)
#install.packages("ggmap")
#install.packages("mapproj")
library(ggmap)
#library(mapproj)

#chosen beta 
beta <- 5

#function that takes in lat/longs and creates a fake outcome
f <- function(latlong) {
  lon_range = range(latlong$lon)
  lat_range = range(latlong$lat)
  lon = ((latlong$lon - lon_range[1])/(lon_range[2]-lon_range[1]))
  lat = ((latlong$lat - lat_range[1])/(lat_range[2]-lat_range[1]))
  outcome <- 3*lon^3 + 3*lat^3
  return(outcome)
}

#reads in Address.csv  
addressData <- read.csv(file = "Addresses.csv", sep = ",", stringsAsFactors = FALSE)
address <- addressData$Addresses

#***********
#reads in latlong.csv which contains the geocoded Addresses
latlong <- read.csv(file = "latlong.csv", sep = ",", stringsAsFactors = FALSE)

#creates random exposure
exposure <- as.data.frame(runif(nrow(addressData), min = 0, max = 1))

#assesses final outcome from latitude/longitude, exposure, beta, and a random error
out <- as.data.frame(f(latlong))
outcome <- as.data.frame(out + exposure*beta + rnorm(nrow(addressData), mean = 0))

#combines address, exposure, and outcome into a data frame
addlatlong <- c(as.data.frame(address), exposure, outcome)
names(addlatlong) <- c("Address", "Exposure", "Outcome")

#writes to csv file 
write.csv(addlatlong, file = "newData.csv", row.names = FALSE)
read.csv("newData.csv")



