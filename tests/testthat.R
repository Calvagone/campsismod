Sys.setenv("R_TESTS" = "")
library(testthat)
library(pmxmod)
test_check("pmxmod")
