
library(testthat)

context("Test model library")

test_that("Templates can be loaded successfully", {
  
  model <- model_suite$nonmem$advan1_trans1
  expect_true(model@parameters %>% contains(Theta(index=1, name="K")))
})
