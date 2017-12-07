select_parents <- function(fitness_values,mechanism=c("rank", "tournament"),random = TRUE, P, c){
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
  #     new_mothers(P x c): Each row is a candidate model for breeding
  
  fitness_rank <- rank(fitness_values)
  fitness_phi <- fitness_rank/sum(fitness_rank)
  parent.pairs <- matrix(rep(0,ceiling(P/2)*2), ncol = 2)
  if (mechanism == "rank"){
    if (random = TRUE){
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
    parent.pairs <- tournament_sample[!duplicated(t(combn(tournament_sample,2)))]
    parent.pairs <- parent.pairs[!rowSums(t(apply(parent.pairs, 1, duplicated))),][1:P,]
  }
  return(parent.pairs)
  
  # point: 
  # 1. after selection, the paired parents should be different.
  # 2. avoid offsprings share the exact same gene.
}