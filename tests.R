#################################################################################
# This file execute all the tests contained in the folders
#################################################################################
library(testthat)
source('./code/utils.R')
sourceEntireFolder('code')

test_dir('tests', filter = NULL, reporter = "summary", env = test_env())

