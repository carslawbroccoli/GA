#################################################################################
# Test the file GeneticAlgorithm.R
#################################################################################

library(testthat)
##### test data #####################
fake_data <- function(c, n, beta_0, beta, sigma){
  # c: number of variables c = 10
  # n: total number of observations
  X <- matrix(rep(round(runif(c, min = 1, max = 10)),n) +
                rnorm(c*n, mean = 0, sd = 0.2), nrow = n, byrow = T)
  Xnames <- paste0("X", 1:c)
  Xdata <- as.data.frame(X)
  colnames(Xdata) <- Xnames
  Y <- rowSums(t(beta*t(X))) + beta_0 + rnorm(n, mean = 0, sd = sigma)
  return(cbind(Xdata, Y))
}

test_data <- fake_data(10, 50, 1,
                       sample(c(round(runif(10/2, min = 2, max = 10)), rep(0,5)), replace = F), 1)




####### unit test for init() #############
context("Test Init() function")

test_that("Repeated initialization does not return the same chromosomes",{
  init1 <- init(test_data, 15, 10)
  init2 <- init(test_data, 15, 10)
  expect_false(identical(init1,init2))
})

test_that("Output is a data.frame", {
  init3 <- init(test_data, 15, 10)
  expect_equal(class(init3),"data.frame")
})
############ unit test for training() ################
context("Test training() function")

test_that("Input errors", {
  init4 <- init(test_data, 15, 10)
  FitnessFunction = "fakeFunction"
  expect_error(training(init4,fitness_function_text=poisson,data= test_data,AIC),
               'argument "X" is missing, with no default')
  expect_error(training(init4,lm,matrix(1:55,5,11),AIC),
               "'data' must be a data.frame, not a matrix or an array")
  expect_error(training(init4,lm,test_data,FitnessFunction),
               "object 'fakeFunction' of mode 'function' was not found")
})

test_that("output is a vector of fitness values", {
  init5 <- init(test_data, 15, 10)
  train1 <- training(init5, lm, test_data,AIC)
  expect_equal(class(train1), "numeric")
  expect_more_than(length(train1), 0)
})

test_that("Algorithm works for both lm/glm",{
  init5 <- init(test_data, 15, 10)
  train1 <- training(init5, lm, test_data,AIC)
  train2 <- training(init5, glm,family= gaussian, test_data,AIC)
  expect_equal(class(train1), "numeric")
  expect_equal(class(train2), "numeric")
})

test_that("Algorithm works for both AIC/BIC",{
  init5 <- init(test_data, 15, 10)
  train1 <- training(init5, lm, test_data,AIC)
  train2 <- training(init5, lm, test_data,BIC)
  expect_equal(class(train1), "numeric")
  expect_equal(class(train2), "numeric")
})
############ unit test for select_parents() ###########
context("Test select_parents() function")
test_that("Input errors", {
  init6 <- init(test_data, 15, 10)
  train6 <- training(init6, lm, test_data,AIC)
  expect_error(select_parents(mechanism="rank", random=T, P=15, c=10),
               'argument "fitness_values" is missing, with no default')
  expect_error(select_parents(train6, mechanism="rank", random=T, c=10),
               'argument "P" is missing, with no default')
})

test_that("number of pairs of parents is ceiling(P/2)", {
  init6 <- init(test_data, 15, 10)
  train6 <- training(init6, lm, test_data,AIC)
  parent6 <- select_parents(train6, mechanism="rank", random=T, 15, 10)
  expect_equal(nrow(parent6), ceiling(15/2))
})

test_that("everytime select_parents() will produce different parent pairs", {
  init6 <- init(test_data, 15, 10)
  train6 <- training(init6, lm, test_data,AIC)
  parent60 <- select_parents(train6, mechanism="rank", random=T, 15, 10)
  parent61 <- select_parents(train6, mechanism="rank", random=T, 15, 10)
  expect_false(identical(parent60, parent61))
})

test_that("difference in mechanism result in same size of parents", {
  init6 <- init(test_data, 15, 10)
  train6 <- training(init6, lm, test_data,AIC)
  parent60 <- select_parents(train6, mechanism="rank", random=T, 15, 10)
  parent61 <- select_parents(train6, mechanism="mechanism", random=T, 15, 10)
  expect_true(identical(dim(parent60), dim(parent61)))
})

########### unit test for breed() function ##########
context("Test breed() function")

test_that("Input Errors", {
  init7 <- init(test_data, 15, 10)
  train7 <- training(init7, lm, test_data,AIC)
  parent7 <- select_parents(train7, mechanism="rank", random=T, 15, 10)
  expect_error(breed(P=15, c=10, parent7, mu=0.5,
                     crossover_points=3, train7, Gap=1/2),"invalid 'times' argument")
  expect_error(breed(init7,P=15, c=10 , mu=0.5,
                     crossover_points=3, train7, Gap=1/2), "invalid 'times' argument")
})

test_that("output is a data.frame with dimension(P,c)", {
  init7 <- init(test_data, 15, 10)
  train7 <- training(init7, lm, test_data,AIC)
  parent7 <- select_parents(train7, mechanism="rank", random=T, 15, 10)
  next_gen7 <- breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                     train7, Gap=1/2)
  expect_equal(class(next_gen7), "data.frame")
  expect_equal(nrow(next_gen7), 15)
  expect_equal(ncol(next_gen7), 10)
})

test_that("everytime repeated crossover and mutation will produce different new generations", {
  init7 <- init(test_data, 15, 10)
  train7 <- training(init7, lm, test_data,AIC)
  parent7 <- select_parents(train7, mechanism="rank", random=T, 15, 10)
  next_gen70 <- breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                      train7, Gap=1/2)
  next_gen71 <- breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                      train7, Gap=1/2)
  expect_false(identical(next_gen70, next_gen71))
})


test_that("generation gap must follow '0< Gap <=1' ", {
  init7 <- init(test_data, 15, 10)
  train7 <- training(init7, lm, test_data,AIC)
  parent7 <- select_parents(train7, mechanism="rank", random=T, 15, 10)
  expect_error(breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                     train7, Gap=0))
  next_gen71 <- breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                      train7, Gap=1/2)
  expect_error(breed(init7,P=15, c=10, parent7, mu=0.5, crossover_points=3,
                     train7, Gap=1.5))
  expect_equal(nrow(next_gen71), 15)
  expect_equal(ncol(next_gen71), 10)
})


########### unit test for get_model() function ##########
context("Test get_model() function")
test_that("output is of the same class as the training method", {
  init8 <- init(test_data, 15, 10)
  train8 <- training(init8, lm, test_data,AIC)
  parent8 <- select_parents(train8, mechanism="rank", random=T, 15, 10)
  next_gen8 <- breed(init8,P=15, c=10, parent8, mu=0.5, crossover_points=3,
                     train8, Gap=1/2)
  new_candidate8 <- get_model(next_gen8, train8, lm, test_data)
  expect_identical(class(new_candidate8), "lm")
})


######### integration test for select() function ###########
context("Integration test for select() function")
test_that("Input errors", {
  expect_error(select(dependent_variable="mpg",P=15, max_iter=100, method_text="lm", fitness_function_text= "AIC", mu=0.1,
                      crossover_points=3, mechanism = "rank",random = FALSE, Gap = 1/4, plot.return = FALSE),
               'argument "df" is missing, with no default')
  expect_error(select(as.matrix(mtcars), dependent_variable="mpg",P=15, max_iter=100, method_text="lm", fitness_function_text= "AIC", mu=0.1,
                      crossover_points=3, mechanism = "rank",random = FALSE, Gap = 1/4, plot.return = FALSE),
               "The input is not a data frame.")
  expect_error(select(mtcars,P=15, max_iter=100, method_text="lm", fitness_function_text= "AIC", mu=0.1,
                      crossover_points=3, mechanism = "rank",random = FALSE, Gap = 1/4, plot.return = FALSE),
               'argument "dependent_variable" is missing, with no default')
})

test_that("Check Output object is a list", {
  result1 <- select(mtcars, "mpg", 15, 200, "lm", "AIC", 0.2, 3, mechanism = "rank",
                    random = FALSE, Gap = 1/4, plot.return = F)
  expect_equal(class(result1), "list")
})

test_that("Check that in some case, maximum number of iterations is useful",{
  result1 <- select(mtcars, "mpg", 15, 200, "lm", "AIC", 0.2, 3, mechanism = "rank",
                    random = FALSE, Gap = 1/4, plot.return = F)
  expect_true(result1$count ==200)
})


test_that("With different mechanism, out algorithm converge to different result",{
  library(MuMIn)
  result11 <- select(mtcars, "mpg", 15, 200, "lm", "BIC", 0.2, 3, mechanism = "rank",
                     random = FALSE, Gap = 1/4, plot.return = F)
  result12 <- select(mtcars, "mpg", 15, 200, "lm", "AICc", 0.2, 3, mechanism = "rank",
                     random = FALSE, Gap = 1/4, plot.return = F)

  expect_false(result12$fitness_value == result11$fitness_value)
  print(result11$model)
  print(result12$model)
})

test_that("when mechanism is 'rank', set 'random=FALSE' may converge to the same result", {
  result11 <-  select(mtcars, "mpg", 15, 200, "lm", "AIC", 0.2, 3, mechanism = "rank",
                      random = FALSE, Gap = 1/4, plot.return = F)
  result12 <-  select(mtcars, "mpg", 15, 200, "lm", "AIC", 0.2, 3, mechanism = "rank",
                      random = TRUE, Gap = 1/4, plot.return = F)
  expect_equal(result11$fitness_value, result12$fitness_value)
})

test_that("select() can generate a plot when setting plot.return=TRUE", {
  result1<- select(mtcars, "mpg", 15, 200, "lm", "AIC", 0.2, 3, mechanism = "rank",
                   random = FALSE, Gap = 1/4, plot.return = T)
  expect_true(identical(class(result1$plot), c("gg", "ggplot")))
})
