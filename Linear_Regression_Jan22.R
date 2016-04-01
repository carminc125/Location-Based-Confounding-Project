#B1: Coefficient
#B2: Coefficient 
#n:  Number of times
#gen_data: creates and returns a list of data points
gen_data <- function(B1, B2, n) {
  e <- rnorm(n, mean = 0) 
  exp1 <- runif(n, min = 0, max = 1)
  exp2 <- runif(n, min = 0, max = 1)
  output <- B1*exp1 + B2*exp2 + e
  return(list("y" = output, "x1" = exp1, "x2" = exp2))
}

#variables B1, B2, oldB1, and oldB2 initialized to enter while loop
#while loop compares oldB1 to the new B1, while checking if the max of the 
#absolute value of the two are less than the threshold.  
fit_model <- function(dlist, thresh) {
  B1 <- 2
  B2 <- 2
  oldB1 <- 0
  oldB2 <- 0
  count <- 0
  x1 <- dlist$x1
  x2 <- dlist$x2
  while(((abs(B1-oldB1)) > thresh || (abs(B2-oldB2)) > thresh) && count < num_iter) {
    oldB1 <- B1
    oldB2 <- B2
    B1 <- solveB(dlist, B2, x2, x1)
    B2 <- solveB(dlist, B1, x1, x2)
    count <- count + 1
  }
  return(list("B1" = B1, "B2" = B2))
}

#Uses coefficients and estimated B to run a linear model and estimate a new B and return it.
solveB <- function(dlist, Bo, expA, expB) {
  r <- dlist$y - Bo*expA
  B <- coef(lm(r ~ expB  -1))
  return(B)
}

