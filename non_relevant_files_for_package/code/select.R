# This file calls the functions from utils to fit a model using genetic algorithm
#source("./code/utils.R")

source("~/repos/GA/code/utils.R")

select <- function(df, dependent_variable, P, max_iter, method_text, fitness_function_text, mu, 
                   crossover_points, mechanism , random = TRUE, Gap = 1/4, plot.return = FALSE){
  # df: data frame
  # dependent_variable: name of y
  # P: number of individuals per generation
  # method: lm/glm
  # fitness_function: AIC/BIC/ow
  # mechanism: "tournament","rank"
  # mu: mutation_rate
  # crossover_points: number of crossover points
  # max_iter: maximum iteration
  c <- ncol(df) - 1
  n <- nrow(df)
  yidx <- which(colnames(df) == dependent_variable)
  Xdata <- df[,-yidx]
  Y <- df[,yidx]
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
      plot_fitness_value <- rbind(plot_fitness_value, cbind(rep((iter+1), P), -candidate_fitness_value))
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
