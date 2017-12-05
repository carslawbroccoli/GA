# Package depends on {stats}, {cvTools} #Find cross-validation, {MuMIn} #Find AICc
library(cvTools)  #Find cross-validation
library(MuMIn) #Find AICc
source("utils.R")

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

################# test init.
P <- 6
initial_generation <- init(test_data, P)


############# test training.
#####################################################

test_fitness_value <- training(initial_generation, lm, test_data, AIC)

which.min(test_fitness_value)

#######################################################
training(initial_generation, lm, test_data, cvLm, K=8)
# linear regression. Cross validation

training(initial_generation, glm, test_data, AIC, family = Gamma)
# glm, family gamma.

################################
######    test select parents.
#################################

parent_1 <- select_parents(test_fitness_value, mechanism = "rank", random = TRUE, P=P, c=c)
# random cannot avoid same parents.
# tournament problems


################################

######    test breed.

#################################

next_gen <- breed(initial_generation,c, parent_1, 0.5, 2, fitness_values = test_fitness_value)

get_model(initial_generation, lm, test_data)
