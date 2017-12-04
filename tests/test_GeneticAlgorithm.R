#################################################################################
# Test the file GeneticAlgorithm.R
#################################################################################
source('./../code/GeneticAlgorithm.R')

test_that("candidates datatype", {
  expect_error(Candidate(X=1), 'X is not of type matrix')
  expect_error(Candidate(X=matrix(1:100,10,10), method = 1), 'method is not of type function')
  expect_error(Candidate(X=matrix(1:100,10,10), method = lm,
                         fitness_function = 1), 'fitness_function is not of type function')
})

test_that("training method for Candidate",{
  
})
