# This file calls the functions from utils to fit a model using genetic algorithm
source("./R/utils.R")

select <- function(df, dependent_variable, P = ceiling(1.5*c), max_iter = 500, method_text = "lm", fitness_function_text = "AIC", mu = 0.1,
                   crossover_points = c-1, mechanism = "rank", random = TRUE, Gap = 1/4, plot.return = FALSE){
  #' @import ggplot2
  #' @name select
  #' @title{select()}
  #' @export
  #' @description This is main call function to run package GA.  This package is comprised of
  #' a main execution file (\emph{select.R} and a R file comtaining all functions
  #' that are necessary for execution (\emph{utils.R}).  The user can enter in a dataset and provide
  #' variables (listed below) to execute the genetic algorithm.
  #' @usage select(df, dependent_variable, P, max_iter, method_text, fitness_function_text, mu,
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
<<<<<<< HEAD
  #' @param dependent_variable (character) Column name of the dependent variable
  #' @param P (numeric) The number of individuals per generation.
  #' @param max_iter (numeric) The maximum number of iterations allowed when running GA
  #' @param method_text (character) "lm" or "glm". methods for fitting the data
  #' @param fitness_function_text (character) name of the fitness function. AIC, BIC, or user defined function.
  #' @param mu (numeric) Mutation rate of the allele within a candidate chromosome.
  #' @param crossover_points (numeric) The number of crossover points during breeding step
  #' @param mechanism (character) The mechanism to select parents. Selection mechanisms are "rank" or "tournament".
=======
  #' @param dependent_variable dependent variable from dataset to fit  with GA
  #' @param P (int) The number of individuals per generation.
  #' @param max_iter (int) The maximum number of iterations allowed when running GA
  #' @param method_text lm() or glm() methods for fitting the data
  #' @param fitness_function_text AIC, BIC, ow
  #' @param mu: Mutation rate of the for each allele within the a given candidate chromosome.
  #' @param crossover_points: (int) The number of crossover points during breeding step
  #' @param mechanism: The mechanism to selection parents by.  Selection mechanisms
  #' are "ranked" or "tournament."
>>>>>>> ac79678360cc366655bd876c9fd375b32b560120
  #' @param random (logical) Random replacement on or off for parent selection
  #' @param Gap Generation gap that determines how parents (generation t) are replaced by offspring of the (t+1) generation
  #' @param plot.return (logical) Boolean for returning plot at end of the algorithm
  #' @examples
  #' select(mtcars, "mpg")
  #'
  #' fake_data <- function(c, n, beta_0, beta, sigma){
  #' # c: number of variables c = 10
  #' # n: total number of observations
  #' X <- matrix(rep(round(runif(c, min = 1, max = 10)),n) + rnorm(c*n, mean = 0, sd = 0.2), nrow = n, byrow = T)
  #' Xnames <- paste0("X", 1:c)
  #' Xdata <- as.data.frame(X)
  #' colnames(Xdata) <- Xnames
  #' Y <- rowSums(t(beta*t(X))) + beta_0 + rnorm(n, mean = 0, sd = sigma)
  #' return(cbind(Xdata, Y))
  #' }
  #'
  #' test_data <- fake_data(10, 50, 1,
  #' sample(c(round(runif(10/2, min = 2, max = 10)), rep(0,5)), replace = F), 1)
  #'
  #' select(test_data, 15, 100, "lm", "AIC", 0.1, 3, mechanism = "rank",
  #' random = FALSE, Gap = 1/4, plot.return = T)
  #'
  c <- ncol(df) - 1
  # number of variables.

  if (!is.data.frame(df)){
    # check if the input is a data frame.
    stop("The input is not a data frame.")
  }

  if (!(dependent_variable %in% colnames(df))){
    # check if the dependent variable is a column name.
    stop("Dependent_variable is not in the data frame. Check the column names of df.")
  }

  # several checks on the value of P.
  if (P < 2){
    stop("The size of generation P should be greater or equal to 2.")
  }
  if (P!=ceiling(P)){
    P=ceiling(P)
    warning("P is not an integer. Use ceiling(P).")
  }
  if ((P < c) | (P > 2*c)){
    warning("The size of generation P is either too large or too small. We recommend setting P between c and 2c.")
  }


  if (!(mechanism %in% c("rank", "tournament"))){
    # check the input of mechanism.
    mechanism = "rank"
    warning("Invalid input for mechanism. Use default \"rank\".")
  }


  n <- nrow(df)
  if (n < c){
    # check if design matrix has full rank.
    warning("The design matrix is not full rank.")
  }

  if (crossover_points > c-1){
    # check the value of crossover points.
    crossover_points <- c-1
    warning("Number of crossover points should not exceed c-1. Continue with c-1.")
  }

  if ( !is.logical(random) ){
    # check if random is logical
    random <- TRUE
    warning("Invalid input for random. Use default \"TRUE\".")
  }

  if ( !is.logical(plot.return) ){
    plot.return <- FALSE
    warning("Invalid input for plot.return. Use default \"FALSE\".")
  }

  # check the value of mu and Gap
  if ( mu < .Machine$double.eps | mu > 1){
    mu <- 0.1
    warning("Mu should be greater or equal to 0 and less or equal to 1. Use default value 0.1.")
  }
  if ( Gap <= .Machine$double.eps | Gap > 1){
    Gap <- 1/4
    warning("Gap should be greater than 0 and less or equal to 1. Use default value 1/4.")
  }

  Yidx <- which(colnames(df) == dependent_variable)
  Y <- df[,Yidx]
  Xdata <- df[,-Yidx]
  df <- cbind(Xdata, Y)
  # note add warning on P. the range of P. P at least greater than 2.
  candidate <- init(df = df, P = P, c = c)
  iter <- 0
  if (plot.return == FALSE){
    min_fitness <- 0
    while ((iter < max_iter) & (sum(min_fitness == min(min_fitness)) < 200)) {
      # stop the iteration if the minimum has not changed for 200 iterations.
      candidate_fitness_value <- training(candidate = candidate, method_text = method_text,
                                          X = df, fitness_function_text= fitness_function_text)
      min_fitness[(iter+1)] <- min(candidate_fitness_value)
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
    return(list(count = iter, model = best_model, fitness_value = min_fitness[iter]))
  }else{
    plot_fitness_value <- NULL
    min_fitness <- 0
    while ((iter < max_iter) & (sum(min_fitness == min(min_fitness)) < 200)) {
      candidate_fitness_value <- training(candidate = candidate, method_text = method_text,
                                          X = df, fitness_function_text= fitness_function_text)
      plot_fitness_value <- rbind(plot_fitness_value, cbind(rep((iter+1), P), -candidate_fitness_value))
      min_fitness[(iter+1)] <- min(candidate_fitness_value)
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
    if( iter >= max_iter){
      warning("Exceed the maximum number of iteration. The model may not give the optimal fitness function value. Try larger maximum of iteration.")
    }
    return(list(count = iter, model = best_model, fitness_value = min_fitness[iter],plot = p))
  }
}

