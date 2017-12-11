#################################################################################
# This file execute all the tests contained in the folders
#################################################################################
library(testthat)
source('./R/utils.R')
sourceEntireFolder('R')

test_dir('tests', filter = NULL, reporter = "summary", env = test_env())

