#######################################################################
#This method creates a set of classes needed for the genetic algorithm
#######################################################################

library(methods)
source('./code/utils.R')


### Class generation contains all the information to breed and create further generation, contains candidate




### Class GeneticAlgorithm contains generations




### Class candidate contains the needed information to train a candidate
Candidate <- function(X = NA, method = NA,
                      fitness_function = NA,
                      candidate = NA){
  # runs tests on the input
  if (class(X)!="matrix"){
    stop('X is not of type matrix')
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
    return(with(object, training(candidate, method, X, fitness_function, ...)))
}
