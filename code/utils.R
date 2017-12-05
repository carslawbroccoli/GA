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

select_parents <- function(fitness_values, P){
  # returns P pairs of parents for breeding
  #   input:
  #     fitness_values (vector P): fitness_value of each of the candidate of the current generation
  #     P (int): number of candidates per generation
  #   output:
  #     parents (matrix P x 2): each row is a pair of indices of parents
  
  #sort fitness values from least to greatest.
  fitness.rank <- sort(fitness_values)
  
  #sum of the fitness ranks
  sum.fitness <- (P + 1) * (P / 2)
  #Probability of Selection for parents based on ranks of fitness values
  #Create vectors of zeros for 
  prob.select <- rep(0, P)
  cumulative.prob <- rep(0, P)
  
  for (i in 1:P) {
    prob.select[i] <- i / sum.fitness
    if (i == 1) {
      cumulative.prob[i] <- prob.select[i]
    }
    else {
      cumulative.prob[i] <- cumulative.prob[i - 1] + prob.select[i]
    }
  }
  
  #create P x 2 matrix for storing parent indices
  parent.pairs <- matrix(rep(0, P*2), nrow = P, ncol = 2)
  for (k in 1:2){
    #Select random starting location
    #Roulette Wheel Selection
    roulette.index <- runif(P, min = 0, max = 1)
    for (i in 1:P) {
      choose <- 0
      for (j in 1:P) {
        if(cumulative.prob[j] < roulette.index[i] 
           & cumulative.prob[j+1] >= roulette.index[i]) {
          parent.pairs[i,k] <- j
          choose <- 1
          break
        }
      }
      if(choose == 0) {
        parent.pairs[i,k] <- i
      }
    }
  }
  
  return(parent.pairs)
}

breed <- function(fitness_values,new_mothers, c, parents, mu, crossover_points, Gap=1/4){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     new_mothers: Each row is a candidate model for breeding
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #     Gap(float): proportion of the generation to be replaced by offspring, 1/p< Gap <=1    
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
    return(offspring)
  }
  
  # Generation Gap
  offspring= mutation(crossover(new_mothers,c, parents, crossover_points), mu)
  if (Gap == 1){
    return(offspring) #return
  }
  else{
    num_replace= floor(P * Gap)
    # assume each time P/2 mother and P/2 father produce P babies
    # num_replace of parents will be replaced by random generated babies
    
    # index of the replaced parents
    replaced_index= sort(fitness_values, index.return= TRUE)$ix[1:num_replace]
    selected_babies= sample(nrow(offspring), size= num_replace, replace = FALSE)
    for (i in 1:length(replaced_index)){
      new_mothers[replaced_index[i]] = offspring[selected_babies[i]]
    }
    return(new_mothers) #retrun
  }
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
}
