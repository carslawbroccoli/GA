<<<<<<< HEAD
# This file contains all the functions needed to run the genetic algorithm 

init <- function(df, P){
  # outputs t P random binary vectors of length c
  # input:
  #   df (data.frame): the datasets with X and Y.
  #   P (int): number of candidates per generations
  # output:
  #   generation(binary matrix P x c): P candidates
  c <- ncol(df) - 1
  return(matrix(c(rep(0,c),sample(c(0,1),(P-2)*c,replace = T),rep(1,c)), nrow = P, byrow = T))
  
  ## points.
  
  # 1. make sure we have all 0 and all 1 in the initial generation.
  # 2. what if weird number of P is entered? say very very small/large P.
  #     - n 
  # return error.
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

select_parents <- function(fitness_values, G, half_random = TRUE){
  # returns Q pairs of parents for breeding
  #   input:
  #     fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #     half_random: select one parent according to fitness rank and another completely random.
  #     G: generation gap.
  #   output:
  #     parents (matrix Q x 2): each row is a pair of indices of parents
  #     new_mothers(P x c): Each row is a candidate model for breeding
  
  return(NULL)
}

breed <- function(new_mothers, c, parents, mu, crossover_points){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     new_mothers: Each row is a candidate model for breeding
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  
  # crossover
  crossover <- function(new_mothers,c, parents, crossover_points){
    k <- sort(sample(1: c-1 , crossover_points)) # crossover point after k-th index
    print(paste("Splitting occurs after position", k))
    # crossover points split the chromosome into parts, 
    # which we can express i-th part as chromosome[k_start[i], k[i]]
    k_start <- c(0, k[-length(k)]) 
    offspring= data.frame()
    
    for (i in 1: nrow(parents)){
      parent1= new_mothers[parents[i, 1], ]
      parent2= new_mothers[parents[i, 2], ] 
      temp1= c()
      temp2= c()
      #for odd j, the j-th part in parent 1 will stay in parent 1, same for part 2
      for (j in 1: length(k)){
        if (j %% 2 ==1){
          temp1= c(temp1, parent1[k_start[j]: k[j]])
          temp2= c(temp2, parent2[k_start[j]: k[j]])
        }
        #for odd j, the j-th part in parent 1 will change to parent 2(same for part 2)
        elif (j %% 2 ==0){
          temp1= c(temp1, parent2[k_start[j]: k[j]])
          temp2= c(temp2, parent1[k_start[j]: k[j]])
        }
      }
      offspring = rbind(offspring, temp1, temp2)
    }
    return(offspring)
  }
  
  
  # mutation
  mutation= function(offspring, mu){
    for (i in 1: nrow(offspring)){
      chromosome= offspring[i, ]
      #generate associated uniform random variable for each locus
      mutationLocus = runif(length(chromosome),0,1)          
      #mutation occurs if r.v. < mutationProbability
      #find the location of mutation
      mutationOccur = mutationLocus < mu       
      #return the final result
      if (length(which(mutationOccur==T)) > 0) {
        print(paste("Mutation occurred at position", which(mutationOccur==T)))
      }        
      offspring[i, ]= (mutationOccur + chromosome) %% 2
    }
  }
  generation= mutation(crossover(P,c, parents, crossover_points), mu)
  return(generation)
}


get_model <- function(candidate, method, X, ...){
  # returns the parameter of the model once we fit method on candidate
  #   input:
  #     candidate (binary vector length c): on or off for each columns of X
  #     method: method for fitting
  #     X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #   output:
  #     lm/glm object : the model selected after GA.
  ynam <- colnames(X)[ncol(X)]
  xnam <- colnames(test_data)[which(as.logical(candidate))]
  fmla <- as.formula(paste( ynam, " ~ ", paste(xnam, collapse= "+")))
  return(method(fmla, data = X,...))
=======
# This file contains all the functions needed to run the genetic algorithm 

init <- function(df, P){
  # outputs t P random binary vectors of length c
  # input:
  #   df (data.frame): the datasets with X and Y.
  #   P (int): number of candidates per generations
  # output:
  #   generation(binary matrix P x c): P candidates
  c <- ncol(df) - 1
  return(matrix(c(rep(0,c),sample(c(0,1),(P-2)*c,replace = T),rep(1,c)), nrow = P, byrow = T))
  
  ## points.
  
  # 1. make sure we have all 0 and all 1 in the initial generation.
  # 2. what if weird number of P is entered? say very very small/large P.
  #     - n 
  # return error.
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

select_parents <- function(fitness_values, P, c){
  # returns P pairs of parents for breeding
  #   input:
  #     fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #     P (int): number of candidates per generation
  #   output:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     new_mothers(P x c): Each row is a candidate model for breeding
  
  return(NULL)
}

breed <- function(new_mothers, c, parents, mu, crossover_points){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     new_mothers: Each row is a candidate model for breeding
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  
  # crossover
  crossover = function(new_mothers,c, parents, crossover_points){
    k <- sort(sample(1: c-1 , crossover_points)) # crossover point after k-th index
    print(paste("Splitting occurs after position", k))
    # crossover points split the chromosome into parts, 
    # which we can express i-th part as chromosome[k_start[i], k[i]]
    k_start <- c(0, k[-length(k)]) 
    offspring= data.frame()
    
    for (i in 1: nrow(parents)){
      parent1= new_mothers[parents[i, 1], ]
      parent2= new_mothers[parents[i, 2], ] 
      temp1= c()
      temp2= c()
      #for odd j, the j-th part in parent 1 will stay in parent 1, same for part 2
      for (j in 1: length(k)){
        if (j %% 2 ==1){
          temp1= c(temp1, parent1[k_start[j]: k[j]])
          temp2= c(temp2, parent2[k_start[j]: k[j]])
        }
        #for odd j, the j-th part in parent 1 will change to parent 2(same for part 2)
        elif (j %% 2 ==0){
          temp1= c(temp1, parent2[k_start[j]: k[j]])
          temp2= c(temp2, parent1[k_start[j]: k[j]])
        }
      }
      offspring = rbind(offspring, temp1, temp2)
    }
    return(offspring)
  }
  
  
  # mutation
  mutation= function(offspring, mu){
    for (i in 1: nrow(offspring)){
      chromosome= offspring[i, ]
      #generate associated uniform random variable for each locus
      mutationLocus = runif(length(chromosome),0,1)          
      #mutation occurs if r.v. < mutationProbability
      #find the location of mutation
      mutationOccur = mutationLocus < mu       
      #return the final result
      if (length(which(mutationOccur==T)) > 0) {
        print(paste("Mutation occurred at position", which(mutationOccur==T)))
      }        
      offspring[i, ]= (mutationOccur + chromosome) %% 2
    }
  }
  generation= mutation(crossover(new_mothers,c, parents, crossover_points), mu)
  return(generation)
}


get_model <- function(candidate, method, X, ...){
  # returns the parameter of the model once we fit method on candidate
  #   input:
  #     candidate (binary vector length c): on or off for each columns of X
  #     method: method for fitting
  #     X (matrix n x (c+1)): data (n x c) and the last column is the value of y.
  #   output:
  #     lm/glm object : the model selected after GA.
  ynam <- colnames(X)[ncol(X)]
  xnam <- colnames(test_data)[which(as.logical(candidate))]
  fmla <- as.formula(paste( ynam, " ~ ", paste(xnam, collapse= "+")))
  return(method(fmla, data = X,...))
>>>>>>> f2e3fea87c6b970b7ed1018cba415def1e02af37
}