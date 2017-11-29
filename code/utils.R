# This file contains all the functions needed to run the genetic algorithm 

init <- function(P, c, collumn_to_use){
  # outputs t P random binary vectors of length c
  # input:
  #   P (int): number of candidates per generations
  #   c (int): number of chromosomes
  #   collumn_to_use(binary vector n): what collumns are allowed
  # output:
  #   generation(binary matrix P x c): P candidates
  
  return(NULL)
}

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

select_parents <- function(fitness_values, P){
  # returns P pairs of parents for breeding
  #   input:
  #     fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #     P (int): number of candidates per generation
  #   output:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  
  return(NULL)
}

breed <- function(parents, mu, crossover_points){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  
  return(NULL)
}