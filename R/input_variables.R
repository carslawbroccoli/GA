# Contains all the input variables of the package

X <- NULL # matrix n x c dataset to be provided by the user
Y <- NULL # vector n  of values provided by the user such that Y is a function of the columns of X

n <- dim(X)[1] # number of datapoints
c <- dim(X)[2] # number of variables

method = lm #method used to fit the problem

#collumns_to_use <- rep(1, c) # binary vector c the ith value is one if the ith column of X is to be used in the fitting, 0 otherwise

crossover_points <- 1 # number of crossoverpoints

G <- 1  #G: Generation Gap, proportion of generation to be replaced by generated offsprings.
#Chapter 3 page 81. section 3.4.2.2.

P <- 2*c # number of candidates per generation

fitness_function = AIC # Either AIC or BIC or other possible fitness function

mu = 0.01 # mutation rate

selection_with_replacement = TRUE # boolean variable set to TRUE if we select parents with replacement

k = 100 # maximum number of generations

eps = 0 # small number, if the difference between the best fitness values of two successive generation is less than eps we consider
        # that the model has converged and we stop
