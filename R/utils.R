#' @title utils.R
#' @export
#' @description This is the R file containing all functions used for the genetic algorithm (GA)
#' package.  An OOP approach was taken to complete this algorithm.

init <- function(df, P, c){
  #' @title init (initialize)
  #' @export
  #' @description function outputs P random binary vectors of length c
  #' @usage init(df, P, c)
  #' @param df (data.frame): the datasets with X and Y.
  #' @param P (int): number of candidates per generations
  #' @param c (int): number of genes per chromosome candidate
  #' @return generation(binary matrix P x c): P candidates
  # outputs t P random binary vectors of length c
  # input:
  #   df (data.frame): the datasets with X and Y.
  #   P (int): number of candidates per generations
  # output:
  #   generation(binary matrix P x c): P candidates
  # c <- ncol(df) - 1
  return(as.data.frame(matrix(c(rep(0,c),sample(c(0,1),(P-2)*c,replace = T),rep(1,c)), nrow = P, byrow = T)))

  ## Notes.

  # 1. make sure we have all 0 and all 1 in the initial generation.
  # 2. what if weird number of P is entered? say very very small/large P.
  #     - n
  # return error.
}


training <- function(candidate, method_text, X, fitness_function_text, ...){
  #' @title training
  #' @export
  #' @description This function fits the method on candidates and return the fitness value of the candidate
  #' @usage training(candidate, method_text, X, fitness_function_text, ...)
  #' @param candidate (binary vector length c): on or off for each columns of X
  #' @param method: method for fitting lm/glm
  #' @param X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #' @param fitness_function_text: type of function to evaluate fitness values
  #' @return fitness_value (float): fitness value of the model
  # fits the method on candidates and return the fitness value of the candidate
  #   input:
  #     candidate (binary vector length c): on or off for each columns of X
  #     method: method for fitting lm/glm
  #     X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #     fitness_function: error of the model
  #   output:
  #     fitness_value (float): fitness value of the model
  method <- match.fun(method_text)
  fitness_function <- match.fun(fitness_function_text)
  individual_training <- function(x){
    ynam <- colnames(X)[ncol(X)]
    if (sum(x)==0){
      fmla <- as.formula(paste(ynam, " ~ 1"))
      return(fitness_function(method(fmla, data = X,...)))
    }else{
      xnam <- colnames(X)[which(as.logical(x))]
      fmla <- as.formula(paste(ynam, " ~ ", paste(xnam, collapse= "+")))
      return(fitness_function(method(fmla, data = X,...)))
    }
  }
  return(apply(candidate, 1, individual_training))
}

select_parents <- function(fitness_values, mechanism, random = random, P, c){
  #' @title select_parents
  #' @export
  #' @description This function returns pairs of parents for breeding
  #' @usage select_parents(fitness_values, mechanism, random = random, P, c)
  #' @param fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #' @param P (int): number of candidates per generation
  #' @param mechanism: user defined rank-based selection mechanism,
  #'         must be one of c("replace_all","tournament", "partial_replace")
  #' @param random: A boolean value(T/F), "T" if choosing 1 parent selected proportional to fitness + 1 parent
  #'         random selected and "F" if 2 parents selected proportional to to fitness
  #' @param generation gap: proportion of the generation to be replaced by offspring
  #'   output:
  #' @return parents (matrix P x 2): each row is a pair of indices of parents
  #' @return candidate(P x c): Each row is a candidate model for breeding
  # returns P pairs of parents for breeding
  #   input:
  #     fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #     P (int): number of candidates per generation
  #     mechanism: user defined rank-based selection mechanism,
  #         must be one of c("replace_all","tournament", "partial_replace")
  #     random: A boolean value(T/F), "T" if choosing 1 parent selected proportional to fitness + 1 parent
  #         random selected and "F" if 2 parents selected proportional to to fitness
  #     Gap: proportion of the generation to be replaced by offspring
  #   output:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     candidate(P x c): Each row is a candidate model for breeding

  fitness_rank <- rank(-fitness_values)
  fitness_phi <- fitness_rank/sum(fitness_rank)
  parent.pairs <- matrix(rep(0,ceiling(P/2)*2), ncol = 2)
  if (mechanism == "rank"){
    i <- 0
    if (random == TRUE){
      while(i <= ceiling(P/2)){
        parent.pairs[i,] <- c(sample.int(P, size = 1,prob = fitness_phi),
                              sample.int(P, size = 1))
        i <- i + 1
      }
    }else{
      while(i <= ceiling(P/2)){
        parent.pairs[i,] <- sample.int(P,size = 2,prob = fitness_phi)
        i <- i + 1
      }
    }
  }else if (mechanism == "tournament"){
    tournament_sample <- NULL
    for (i in 1:(2*ceiling(P/2))){
      tournament_sample[i] <- which.max(fitness_rank[sample(1:P, size = max(5,ceiling(P/4)), replace = T)])
    }
    parent.pairs <- matrix(tournament_sample, byrow = T, ncol = 2)
  }
  return(parent.pairs)
}

breed <- function(candidate, c, P, parent.pairs, mu, crossover_points, fitness_values, Gap){
  #' @title breed
  #' @export
  #' @description This function returns P candidates of the next generation based on the pairs of parents.
  #' Crossover and mutation is also contained within this function.
  #' @usage breed(candidate, c, P, parent.pairs, mu, crossover_points, fitness_values, Gap)
  #' @param candidate Each row of this matrix corresponds to a candidate model of current generation.
  #' @param c The number of chromosomes for each candidate function.
  #' @param parent.pairs Matrix of parent breeding pairs, a result of the \emph{select_parents} function.
  #' @param mu (float) This is the mutation rate of chromosomes in each candidate function
  #' @param crossover_points Number of crossover points to be used in the bredding step.
  #' @param fitness_values These are calculated fitness values of the present generation.
  #' This comes in the form of a vector, and fitness values at a particular index corresponds
  #' to the candidate function of the same index in the candidate function
  #' @param Gap Generation gap for replacement of parents with offspring from each
  #' created iteration of the GA
  #' @return generation(binary matrix P x c): P candidates
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     candidate: Each row is a candidate model for breeding
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  # crossover
  # add if crossoverpoint = 0, skip crossover part.
  crossover <- function(candidate, c, parent.pairs, crossover_points){
    offspring <- as.data.frame(matrix(rep(0, nrow(parent.pairs)*2*c), ncol = c))
    for (j in 1:nrow(parent.pairs)) {
      temp1 <-  candidate[parent.pairs[j,1],]
      temp2 <-  candidate[parent.pairs[j,2],]
      pos <- c(0, sort(sample(1:(c-1), crossover_points, replace = F)), c)
      pos_interval <- unname(split(1:c, rep(1:length(diff(pos)), diff(pos))))
      change_idx <- (1:((crossover_points+1)/2))*2
      for (i in 1:length(change_idx)){
        temp <- temp1[pos_interval[[change_idx[i]]]]
        temp1[pos_interval[[change_idx[i]]]] <- temp2[pos_interval[[change_idx[i]]]]
        temp2[pos_interval[[change_idx[i]]]] <- temp
      }
      offspring[(2*j-1),] <- temp1
      offspring[(2*j),] <- temp2
    }
    # notes: input 1 <= crossover_points <= c-1. else return error. warning("crossover_point not proper")
    #print(paste("Splitting occurs after position", k))
    # crossover points split the chromosome into parts,
    # which we can express i-th part as chromosome[k_start[i], k[i]]
    return(offspring)
  }

  #' @title mutation
  #' @export
  #' @usage mutation(offspring, mu)
  #' @param offspring (data frame) contains the offspring produced from crossover
  #' @param mu Mutation rate of the each allele within an offspring
  #' @return offspring (data frame) Offspring data frame after mutation
  # mutation
  mutation <- function(offspring, mu){
    prob_mutation <- runif(length(offspring),0,1)
    offspring[which(prob_mutation < mu)] <- abs(offspring[which(prob_mutation < mu)] - 1)
    return(offspring)
  }

  # Generation Gap
  offspring <- mutation(crossover(candidate,c, parent.pairs, crossover_points), mu)
  if (Gap == 1){
    return(offspring) #return
  }else{

    num_replace <- ceiling(P * Gap)
    # assume each time P/2 mother and P/2 father produce P babies
    # num_replace of parents will be replaced by random generated babies

    # index of the replaced parents
    replaced_index <- sort(fitness_values, decreasing = T,index.return= TRUE)$ix[1:num_replace]
    selected_babies <- sample(nrow(offspring), size= num_replace, replace = FALSE)
    candidate[replaced_index,] <- offspring[selected_babies,]
    return(candidate) #return
  }
}

get_model <- function(candidate, fitness_values, method_text, X, ...){
  #' @title get_model
  #' @export
  #' @description This function returns the parameter of the model once we fit method on candidate
  #' @usage get_model(candidate, fitness_values, method_text, X)
  #' @param candidate (binary vector length c): on or off for each columns of X
  #'     method: method for fitting
  #' @param X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #' @return lm/glm object : the model selected after GA.
  # returns the parameter of the model once we fit method on candidate
  #   input:
  #     candidate (binary vector length c): on or off for each columns of X
  #     method: method for fitting
  #     X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #   output:
  #     lm/glm object : the model selected after GA.
  method <- match.fun(method_text)

  best <- candidate[which.min(fitness_values),]
  ynam <- colnames(X)[ncol(X)]
  if (sum(best)==0){
    fmla <- as.formula(paste(ynam, " ~ 1"))
    return(method(fmla, data = X,...))
  }else{
    xnam <- colnames(X)[which(as.logical(best))]
    fmla <- as.formula(paste( ynam, " ~ ", paste(xnam, collapse= "+")))
    return(method(fmla, data = X,...))
  }
}
