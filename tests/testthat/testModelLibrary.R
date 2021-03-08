
library(testthat)

context("Test model library")

test_that("Templates can be loaded successfully", {
  
  model <- getNONMEMModelTemplate(1, 1)
  expect_true(model@parameters %>% contains(Theta(index=1)))
})