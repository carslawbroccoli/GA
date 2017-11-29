# This file calls the functions from utils to fit a model using genetic algorithm
source("utils.R")


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