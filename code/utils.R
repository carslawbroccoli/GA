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

## finds all .R files within a folder and soruces them
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}

create_dataset <- function(){
  #######################
  ## fake data generation.
  #######################
  c <- 10
  # number of variables c = 10
  n <- 30
  # total number of observations
  X <- matrix(rep(round(runif(10, min = 1, max = 10)),n) + rnorm(c*n, mean = 0, sd = 0.2), nrow = n, byrow = T)
  # matrix of variables
  Xdata <- as.data.frame(X)
  # matrix to a data frame.
  Xnames <- paste0("X", 1:c)
  colnames(Xdata) <- Xnames
  # rename the columns of the data frame.
  
  beta <- round(runif(10, min = 2, max = 10))
  beta[sample(1:10,5)] = 0
  # slope
  beta_0 <- 7
  # intercept
  
  Y <- rowSums(t(beta*t(X))) + beta_0 + rnorm(n, mean = 0, sd = 0.5)
  # Y values.
  
  test_data <- cbind(Xdata, Y)
  # combine the X and Y values into a data frame.
  
  #######################################################
  
  # generate a single candidate
  chromo_test_vec <- sample(c(0,1),c, replace = T)
  
  obj <- list(candidate = chromo_test_vec,
              X = test_data)
}
