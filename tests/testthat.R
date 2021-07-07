Sys.setenv("R_TESTS" = "")
library(testthat)
library(campsismod)
test_check("campsismod")
