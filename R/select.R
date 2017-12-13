# This file calls the functions from utils to fit a model using genetic algorithm
#source("./code/utils.R")
#library('ggplot2')
source("~/GA/R/utils.R")
source("~/GA/R/input_variables.R")

select <- function(df, P, max_iter, method_text, fitness_function_text, mu,
                   crossover_points, mechanism , random = TRUE, Gap = 1/4, plot.return = FALSE){
  #' @importFrom stats as.formula runif
  #' @name select
  #' @title{select()}
  #' @export
  #' @description This is main call function to run package GA.  This package is comprised of
  #' a main execution file (\emph{select.R} and a R file comtaining all functions
  #' that are necessary for execution (\emph{utils.R}).  The user can enter in a dataset and provide
  #' variables (listed below) to execute the genetic algorithm.
  #' @usage select(df, P, max_iter, method_text, fitness_function_text, mu,
  #' crossover_points, mechanism, random = TRUE, Gap = 1/4, plot.return = FALSE)
  #' @details Contained in the list below are the invdividual functions that are called during the
  #' execution of the genetic algorithm.
  #' \itemize{
  #'  \item{init()}: {Initializes the dataframe for the dataset}
  #'  \item{training()}: {fits the method on candidates and return the fitness value of the candidate}
  #'  \item{select_parents()}: {Chooses parents for breeding based off of ranked
  #'  or tournament selection using the fitness values of each parent}
  #'  \item{breed()}: {Breeds parents based off of pairing from select_parents()}
  #'  \item{crossover()}: {Function within breed() to conduct crossover between parent pairs
  #'  during breeding step}
  #'  \item{mutation()}: {Determines if alleles of offspring (t+1 generation) mutates or not}
  #'  \item{get_model()}: {Returns the best fit model of the dataset}
  #' }
  #' @param df (data frame) Dataset to fit
  #' @param P (int) The number of individuals per generation.
  #' @param max_iter (int) The maximum number of iterations allowed when running GA
  #' @param method_text lm() or glm() methods for fitting the data
  #' @param fitness_function_text AIC, BIC, ow
  #' @param mu: Mutation rate of the for each allele within the a given candidate chromosome.
  #' @param crossover_points: (int) The number of crossover points during breeding step
  #' @param mechanism: The mechanism to selection parents by.  Selection mechanisms
  #' are "ranked" or "tournament."
  #' @param random (logical) Random replacement on or off for parent selection
  #' @param Gap Generation gap that determines how parents (generation t) are replaced by offspring
  #' of the (t+1) generation
  #' @param plot.return (logical) Boolean for returning plot at end of the algorithm
  c <- ncol(df) - 1
  n <- nrow(df)
  Xdata <- df[,1:c]
  Y <- df[,c]
  # note add warning on P. the range of P. P at least greater than 2.
  candidate <- init(df = df, P = P, c = c)
  iter <- 0
  if (plot.return == FALSE){
    min_fitness <- 0
    while ((iter < max_iter) & (sum(min_fitness == min(min_fitness)) < 100)) {
      candidate_fitness_value <- training(candidate = candidate, method_text = method_text,
                                          X = df, fitness_function_text= fitness_function_text)
      min_fitness[(iter+1)] <- min(candidate_fitness_value)
      bn <- mean(min_fitness^2) - mean(min_fitness)^2
      candidate_parents <- select_parents(fitness_values = candidate_fitness_value,
                                          mechanism = mechanism, random = random, P = P, c = c)
      candidate <- breed(candidate = candidate, c = c, P = P, parent.pairs = candidate_parents,
                         mu = mu, crossover_points = crossover_points,
                         fitness_values = candidate_fitness_value, Gap = Gap)
      iter <- iter + 1
    }
    candidate_fitness_value <- training(candidate = candidate, method_text = method_text, X = df,
                                        fitness_function_text= fitness_function_text)
    best_model <- get_model(candidate = candidate, fitness_values = candidate_fitness_value,
                            method_text = method_text, X = df)
    return(list(count = iter, model = best_model))
  }else{
    plot_fitness_value <- NULL
    min_fitness <- 0
    while ((iter < max_iter) & (sum(min_fitness == min(min_fitness)) < 100)) {
      candidate_fitness_value <- training(candidate = candidate, method_text = method_text,
                                          X = df, fitness_function_text= fitness_function_text)
      plot_fitness_value <- rbind(plot_fitness_value, cbind(rep((iter+1), P), -1*candidate_fitness_value))
      min_fitness[(iter+1)] <- min(candidate_fitness_value)
      bn <- mean(min_fitness^2) - mean(min_fitness)^2
      candidate_parents <- select_parents(fitness_values = candidate_fitness_value,
                                          mechanism = mechanism, random = random, P = P, c = c)
      candidate <- breed(candidate = candidate, c = c, P = P, parent.pairs = candidate_parents,
                         mu = mu, crossover_points = crossover_points,
                         fitness_values = candidate_fitness_value, Gap = Gap)
      iter <- iter + 1
    }
    plot_fitness_value <- as.data.frame(plot_fitness_value)
    colnames(plot_fitness_value) <- c("generation", "fitness_value")
    p <- ggplot(plot_fitness_value, aes(generation, fitness_value)) +
      geom_point() + labs(y = paste("Negative", fitness_function_text), x = "Generation")
    print(p)
    candidate_fitness_value <- training(candidate = candidate, method_text = method_text,
                                        X = df, fitness_function_text= fitness_function_text)
    best_model <- get_model(candidate = candidate, fitness_values = candidate_fitness_value,
                            method_text = method_text, X = df)
    return(list(count = iter, model = best_model, plot = p))
  }
}
