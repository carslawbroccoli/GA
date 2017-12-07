source("./R/utils.R")
#' @title main.R
#' @description This file calls the functions from utils to fit a model using genetic algorithm
#' Takes all of the components of the from utils and runs the full algorithm on the input data.
#' @param P (int) number of candidates per generation
#' @param c (int) number of chromosomes per candidate solution
#' @param collumn_to_use Not sure what this is used for
#' @param k (int) total number of generations
#' @param eps (float) epsilon convergence condition, will exit algorithm if this condition is
#' satisfied before total number of generations is reached
#' @param X raw data to be fit
#' @param Y output values of function
#' @param fitness_function type of function used to compute the fitness of each candidate.
#' Either AIC or BIC is used in this package.
#' @return model Final model output from running GA


main <- function(P, c, collumn_to_use, k, eps, method, X, Y, fitness_function){
  generation <- init(P, c, collumn_to_use)
  cpt <- 1
  delta = Inf
  best_fitness_val = Inf
  while(cpt <= k & delta > eps){
    fitness_values <- apply(generation, 1, function(candidate){utils.training(candidate, method, X, Y, fitness_function)})
    parents <- select_parents (fitness_values, P)
    if(cpt<k){
      generation <- breed(parents, mu, crossover_points)
    }
    cpt <- cpt+1
    delta = abs(best_fitness_val - min(fitness_values))
    best_fitness_val <- min(fitness_values)
  }

  idx <- which.min(fitness_values)
  model <- utils.get_model(generation[idx], method, X, Y)
  return(model)
}
