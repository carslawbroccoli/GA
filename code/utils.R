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

breed <- function(P, c, parents, mu, crossover_points){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  
  # crossover
  crossover = function(P,c, parents, crossover_points){
    k <- sort(sample(1: c-1 , crossover_points)) # crossover point after k-th index
    print(paste("Splitting occurs after position", k))
    # crossover points split the chromosome into parts, 
    # which we can express i-th part as chromosome[k_start[i], k[i]]
    k_start <- c(0, k[-length(k)]) 
    offspring= data.frame()
    
    for (i in 1: nrow(parents)){
      parent1= P[parents[i, 1]]
      parent2= P[parents[i, 2]] 
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