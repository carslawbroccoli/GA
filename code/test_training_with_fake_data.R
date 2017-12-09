# Package depends on {stats}, {cvTools} #Find cross-validation, {MuMIn} #Find AICc
library(cvTools)  #Find cross-validation
library(MuMIn) #Find AICc

source("~/repos/GA/code/utils.R")

#######################
## fake data generation.
#######################


fake_data <- function(c, n, beta_0, beta, sigma){
  # c: number of variables c = 10
  # n: total number of observations
  X <- matrix(rep(round(runif(c, min = 1, max = 10)),n) + rnorm(c*n, mean = 0, sd = 0.2), nrow = n, byrow = T)
  Xnames <- paste0("X", 1:c)
  Xdata <- as.data.frame(X)
  colnames(Xdata) <- Xnames
  Y <- rowSums(t(beta*t(X))) + beta_0 + rnorm(n, mean = 0, sd = sigma)
  return(cbind(Xdata, Y))
}

test_data <- fake_data(10, 50, 1, 
                       sample(c(round(runif(10/2, min = 2, max = 10)), rep(0,5)), replace = F), 1)

main(test_data, 15, 100, "lm", "cvLm", 0.1, 3, mechanism = "rank", 
     random = FALSE, Gap = 1/4, plot.return = T, K = 5)


