tournament_sample <- rep(0,P)

fitness_rank <- rank(fit_val)

parent.pairs <- matrix(rep(0, P), ncol=2)

for (j in 1:2){
for (i in 1:(P/2)){
  #create tournament population to compete
  #indices from fitness value vector
  tournament.pop <- sample(P, size=10, replace = TRUE)
  #find most fit candidate from the tournament population
  #yields indice pointing to most fit parent candidate in pop
  best <- tournament.pop[which.max(fitness_values[tournament.pop])]
  tournament_sample[i] <- best
}
parent.pairs[,j] <- tournament_sample
}
