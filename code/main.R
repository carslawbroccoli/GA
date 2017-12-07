# This file calls the functions from utils to fit a model using genetic algorithm
source("./code/utils.R")



main <- function(df, P, max_iter,method, fitness_function, mu, crossover_points, mechanism , random = TRUE, Gap = 1/4){
  # df: data frame
  # P: number of individuals per generation
  # method: lm/glm
  # fitness_function: AIC/BIC/ow
  # mechanism: "tournament","rank"
  # mu: mutation_rate
  # crossover_points: number of crossover points
  # max_iter: maximum iteration
  c <- ncol(df) - 1
  n <- nrow(df)
  Xdata <- df[,1:c]
  Y <- df[,c]
  # note add warning on P. the range of P. P at least greater than 2.
  candidate <- init(df = df, P = P, c = c)
  iter <- 0
  while (iter < max_iter) {
    candidate_fitness_value <- training(candidate = candidate, method = method, X = df, fitness_function= AIC)
    candidate_parents <- select_parents(fitness_values = candidate_fitness_value, 
                                        mechanism = mechanism, random = random, P = P, c = c)
    candidate <- breed(candidate = candidate, c = c, P = P, parent.pairs = candidate_parents, 
                       mu = mu, crossover_points = crossover_points, 
                       fitness_values = candidate_fitness_value, Gap = Gap)
    iter <- iter + 1
  }
  candidate_fitness_value <- training(candidate = candidate, method = method, X = df, fitness_function= AIC)
  best_model <- get_model(candidate = candidate, fitness_values = candidate_fitness_value, method = method, X = df)
  return(best_model)
}
