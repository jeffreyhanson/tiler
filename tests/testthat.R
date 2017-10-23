# set environmental variable
Sys.setenv("R_TESTS" = "")

# load packages
library(testthat)
library(tiler)

# run tests
test_check("tiler")
