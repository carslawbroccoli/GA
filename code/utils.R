init <- function(df, P){
  # outputs t P random binary vectors of length c
  # input:
  #   df (data.frame): the datasets with X and Y.
  #   P (int): number of candidates per generations
  # output:
  #   generation(binary matrix P x c): P candidates
  c <- ncol(df) - 1
  return(as.data.frame(matrix(c(rep(0,c),sample(c(0,1),(P-2)*c,replace = T),rep(1,c)), nrow = P, byrow = T)))
  
  ## Notes.
  
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

select_parents <- function(fitness_values, mechanism=c("rank", "tournament"),random = TRUE, P, c){
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
  
  
  fitness_rank <- rank(fitness_values)
  fitness_phi <- fitness_rank/sum(fitness_rank)
  parent.pairs <- matrix(rep(0,ceiling(P/2)*2), ncol = 2)
  if (mechanism == "rank"){
    if (random == TRUE){
      i <- 0
      while(i <= ceiling(P/2)){
        parent.pairs.candidate <- c(sample.int(P, size = 1,prob = fitness_phi),
                                    sample.int(P, size = 1))
        if (sum(apply(parent.pairs, 1, identical, parent.pairs.candidate)) == 0){
          parent.pairs[i,] <- parent.pairs.candidate
          i <- i + 1
        }
      }
    }else{
      while(i <= ceiling(P/2)){
        parent.pairs.candidate <- sample.int(P,size = 2,prob = fitness_phi)
        if (sum(apply(parent.pairs, 1, identical, parent.pairs.candidate)) == 0){
          parent.pairs[i,] <- parent.pairs.candidate
          i <- i + 1
        }
      }
    }
  }else if (mechanism == "tournament"){
    tournament_sample <- rep(0,P)
    for (i in 1:P){
      tournament_sample[i] <- which.max(fitness_rank[sample.int(P, size = ceiling(P/4), replace = T)])
    }
    #build output matrix of selected parents for breeding
    parent.pairs <- tournament_sample[!duplicated(t(combn(tournament_sample,2)))]
    parent.pairs <- parent.pairs[!rowSums(t(apply(parent.pairs, 1, duplicated))),][1:P,]
  }
  #returns the indices of parents from the original list
  return(parent.pairs)
  
  # point: 
  # 1. after selection, the paired parents should be different.
  # 2. avoid offsprings share the exact same gene.
}

breed <- function(candidate, c, parent.pairs, mu, crossover_points, fitness_values, Gap=1/4){
  # returns P candidates of the next generation based on the pairs of parents
  #   input:
  #     candidate: Each row is a candidate model for breeding
  #     parents (matrix P x 2): each row is a pair of indices of parents
  #     mu (float): mutation rate
  #     crossover_points (int): number of crossover points
  #   output:
  #     generation(binary matrix P x c): P candidates
  # crossover
  crossover <- function(candidate, c, parent.pairs, crossover_points){

    pos <- sort(sample(1:(c-1), crossover_points, replace = F))
    k <- unname(split(1:c, cumsum(seq_along(1:c) %in% pos))) # crossover point after k-th index
    # notes: input 1 <= crossover_points <= c-1. else return error. warning("crossover_point not proper")
    #print(paste("Splitting occurs after position", k))
    # crossover points split the chromosome into parts, 
    # which we can express i-th part as chromosome[k_start[i], k[i]]
    offspring= data.frame()
    
    for (i in 1: nrow(parent.pairs)){
      parent1= candidate[parent.pairs[i, 1], ]
      parent2= candidate[parent.pairs[i, 2], ] 
      temp1= c()
      temp2= c()
      #for odd j, the j-th part in parent 1 will stay in parent 1, same for part 2
      for (j in 1:(crossover_points + 1)){
        if (j %% 2 ==1){
          temp1= c(temp1, parent1[k[[j]]])
          temp2= c(temp2, parent2[k[[j]]])
        }
        #for odd j, the j-th part in parent 1 will change to parent 2(same for part 2)
        else if (j %% 2 ==0){
          temp1= c(temp1, parent2[k[[j]]])
          temp2= c(temp2, parent1[k[[j]]])
        }
      }
      offspring = rbind(offspring, temp1, temp2)
    }
    return(offspring)
  }
  
 
}
