#################################################################################
# This file execute all the tests contained in the folders
#################################################################################
library(testthat)
sourceEntireFolder('R')

test_dir('tests', filter = NULL, reporter = "summary", env = test_env())

