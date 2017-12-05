#################################################################################
# Test the file GeneticAlgorithm.R
#################################################################################
source('./../code/GeneticAlgorithm.R')

test_that("candidates datatype", {
  expect_error(Candidate(X=1), 'X is not of type data.frame')
  expect_error(Candidate(X=matrix(1:100,10,10), method = 1), 'method is not of type function')
  expect_error(Candidate(X=matrix(1:100,10,10), method = lm,
                         fitness_function = 1), 'fitness_function is not of type function')
})

test_that("training method for Candidate",{
  set.seed(1)
  fake_data = create_dataset()
  fake_candidate <- Candidate(fake_data$X, lm, AIC, fake_data$candidate)
  expect_equal(training.Candidate(fake_candidate), 120.013031282454)
})
