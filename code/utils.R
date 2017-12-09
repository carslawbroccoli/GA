init <- function(df, P, c){
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
  
  # point: 
  # 1. after selection, the paired parents should be different.
  # 2. avoid offsprings share the exact same gene.
}

breed <- function(candidate, c, P, parent.pairs, mu, crossover_points, fitness_values, Gap){
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
  
  
  # mutation
  mutation <- function(offspring, mu){
    for (i in 1: nrow(offspring)){
      chromosome <-  offspring[i, ]
      #generate associated uniform random variable for each locus
      mutationLocus <-  runif(length(chromosome),0,1)          
      #mutation occurs if r.v. < mutationProbability
      #find the location of mutation
      mutationOccur <- (mutationLocus < mu)       
      #return the final result
      offspring[i, ] <- (mutationOccur + chromosome) %% 2
    }
    return(offspring)
  }
  
  # Generation Gap
  offspring <- mutation(crossover(candidate,c, parent.pairs, crossover_points), mu)
  if (Gap == 1){
    return(offspring) #return
  }else{
    num_replace= floor(P * Gap)
    # assume each time P/2 mother and P/2 father produce P babies
    # num_replace of parents will be replaced by random generated babies
    
    # index of the replaced parents
    replaced_index= sort(fitness_values, decreasing = T,index.return= TRUE)$ix[1:num_replace]
    selected_babies= sample(nrow(offspring), size= num_replace, replace = FALSE)
    candidate[replaced_index,] <- offspring[selected_babies,]
    return(candidate) #return
  }
}

get_model <- function(candidate, fitness_values, method_text, X, ...){
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