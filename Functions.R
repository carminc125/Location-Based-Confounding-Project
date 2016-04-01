#takes in a data file containing addresses, exposure, and outcomes 
#converts addresses to lat/long and evaluates new outcomes
#returns a data frame 
#*************
readData <-function(addressFile = "newData.csv") {
  addData <- read.csv(file = addressFile, sep = ",", stringsAsFactors = FALSE)
  latlong <- read.csv(file = "latlong.csv", sep = ",", stringsAsFactors = FALSE)
  data <- data.frame("lat" = latlong$lat, "long" = latlong$lon, "x1" = addData$Exposure, "outcome" = addData$Outcome)
  return(data)
}

#iterates to estimate f and B until convergence 
fitModel <- function(trainingData, theta, lambda, thresh = 0.0002) {
  n <- length(trainingData$outcome)
  
  oldB <- 0
  oldf <- rep(0, n) 
  newB <- 1
  newf <- rep(1, n)
  while (abs(oldB-newB) > thresh || max(abs(oldf-newf)) > thresh) {
    oldB <- newB
    oldf <- newf
    f <- solveF(trainingData, oldB, theta, lambda)
    newf <- predict(f, trainingData$latlong)
    newB <- solveB(trainingData, f)
  }
  return(list("f" = f, "b" = newB)) #TRAINING DATA
}

#function will give us estimated f
solveF <- function (trainingData, B, theta, lambda) {
  r <- trainingData$outcome - B*trainingData$x1
  fit <- fastTps(trainingData$latlong, r, theta = theta, lambda = lambda)
  return(fit)
}

#function will give us estimated B
solveB <- function (trainingData, f) {
  r <- trainingData$outcome - predict(f)
  B <- coef(lm(r ~ trainingData$x1 - 1))
}


#Takes in estimated lambdas, produces models from it and calls the predict function to estimate outcome
#Compares these outcomes to real outcomes by doing a sum of squares regression
evaluateModel <- function(trainingData, testLambda, testData) {
  testModel <- fitModel(trainingData, theta, testLambda, thresh = 0.0002)
  estOutcome <- predictModel(testModel, testData)
  finalPrediction <- ((estOutcome - testData$outcome)^2)
  finalSum <- sum(finalPrediction)
  return(finalSum)
}

#evaluate models with lambda training data, test data, and lambdas and calls evaluate model
trainingLambdas <- function(fitLambda, trainingData, testData) {
  values <- rep(0, length(fitLambda))
  for (i in 1:length(fitLambda)) {
    values[i] <-  evaluateModel(trainingData, fitLambda[i], testData) 
    print(values[i])
  }
  spot <- which.min(values)
  print(fitLambda[spot])
}


#takes in an estimated model, and returns an estimated outcome
predictModel <- function(testModel, testData) {
 value <- predict(testModel$f, testData$latlong)
 outcome <- value + testModel$b*testData$x1
 return(outcome)
}

