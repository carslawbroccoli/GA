#######################################################################
#' This method creates a set of classes needed for the genetic algorithm
#######################################################################

library(methods)
source('./R/utils.R')


### Class generation contains all the information to breed and create further generation, shuld contain a list of candidate and
# all the information to create the next gen




### Class GeneticAlgorithm should have a field generation and the condition for the algo to stop




### Class candidate contains the needed information to train a candidate
Candidate <- function(X = NA, method = NA,
                      fitness_function = NA,
                      candidate = NA){
  # runs tests on the input
  if (class(X)!="data.frame"){
    stop('X is not of type data.frame')
  }
  if (class(method)!="function"){
    stop('method is not of type function')
  }
  if (class(fitness_function)!="function"){
    stop('fitness_function is not of type function')
  }
  if (class(fitness_function)!="function"){
    stop('fitness_function is not of type function')
  }
  warning('needs test for candidate type')
  warning('need to test the size of `candidate` and compare it to the width of `X`')


  # create the candidate
  obj <- list(X = X,
              Y = Y,
              method = method,
              fitness_function = fitness_function,
              candidate = candidate)
  class(obj) <- 'Candidate'
  return(obj)
}

training.Candidate <- function(object){
    return(with(object, training(candidate, method, X, fitness_function)))
}
