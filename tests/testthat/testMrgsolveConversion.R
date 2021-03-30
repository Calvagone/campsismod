
library(testthat)

context("Test the conversion to mrgsolve")

test_that("Export method", {
  model <- getNONMEMModelTemplate(4,4)
  
  mrgsolve <- model %>% export(dest="mrgsolve")
})