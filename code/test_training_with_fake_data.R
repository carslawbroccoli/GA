# Package depends on {stats}, {cvTools} #Find cross-validation, {MuMIn} #Find AICc
library(cvTools)  #Find cross-validation
library(MuMIn) #Find AICc

#######################
## fake data generation.
#######################
c <- 10
# number of variables c = 10
n <- 30
# total number of observations
X <- matrix(rep(round(runif(10, min = 1, max = 10)),n) + rnorm(c*n, mean = 0, sd = 0.2), nrow = n, byrow = T)
# matrix of variables
Xdata <- as.data.frame(X)
# matrix to a data frame.
Xnames <- paste0("X", 1:c)
colnames(Xdata) <- Xnames
# rename the columns of the data frame.

beta <- round(runif(10, min = 2, max = 10))
beta[sample(1:10,5)] = 0
# slope
beta_0 <- 7
# intercept

Y <- rowSums(t(beta*t(X))) + beta_0 + rnorm(n, mean = 0, sd = 0.5)
# Y values.

test_data <- cbind(Xdata, Y)
# combine the X and Y values into a data frame.

#######################################################

# generate a single candidate
chromo_test_vec <- sample(c(0,1),c, replace = T) # large size but easy to code

#####################################################
  
training <- function(candidate, method, X, fitness_function, ...){
  # fits the method on candidates and return the fitness value of the candidate
  #   input:
  #     candidate (binary vector length c): on or off for each columns of X
  #     method: method for fitting lm/glm
  #     X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #     fitness_function: error of the model
  #   output:
  #     fitness_value (float): fitness value of the model
  ynam <- colnames(X)[ncol(X)]
  xnam <- colnames(test_data)[which(as.logical(candidate))]
  fmla <- as.formula(paste( ynam, " ~ ", paste(xnam, collapse= "+")))
  return(fitness_function(method(fmla, data = X,...)))
}

######################################################

training(chromo_test_vec, lm, test_data, AIC)
# linear regression. AIC

training(chromo_test_vec, lm, test_data, AICc)
# linear regression. AICc


training(chromo_test_vec, lm, test_data, cvLm, K=8)
# linear regression. Cross validation


training(chromo_test_vec, glm, test_data, AIC, family = Gamma)
# glm, family gamma.
