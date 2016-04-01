setwd("/Users/carmincovarrubias/Dropbox/R-Script/")
rm(list=ls())
source("Partly_Linear_Feb5.R")
install.packages("fields")
library(fields)
install.packages("ggmap")
install.packages("mapproj")
library(ggmap)
library(mapproj)

#parameters
theta <- 10
fitLambda <- (2^(-10:20))

#will split data into two frames, training data and test data 
#splitData <- function(data) {
#}

#Turns out data into a list and lat/long into dataframe (required format for program)
listConvert <- function(data) {
  newData <- list("latlong" = data.frame(data$lat, data$long), "x1" = data$x1, "outcome" = data$outcome)
  return(newData)
}

set.seed(1)
#turns addresses into lat/longs, returns as data.frame
data <- readData("newData.csv")
#data <- data[-11,]

#splits our data into two halves then converts to correct list format for training and test data
split1 <- data[1:31,]
split2 <- data[32:62, ]
trainDat <- listConvert(split1)
testDat <- listConvert(split2)

#evaluate models with training data, test data, and lambda guesses
finalLambda <- trainingLambdas(fitLambda, trainDat, testDat)

#fits a model with the final lambda values, runs a prediction of outcomes
trainingModel <- fitModel(trainDat, theta, finalLambda, thresh = 2e-04)
tModelPredict <- predictModel(trainingModel, testDat)

#fitted <- fitModel((listConvert(data)), theta, finalLambda, thresh = 2e-04)

