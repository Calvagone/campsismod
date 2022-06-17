library(testthat)

context("Test the generic list")

source(paste0("", "testUtils.R"))

test_that("GetByIndex method works well", {
  model <- model_suite$nonmem$advan4_trans4
  
  depot <- model@compartments %>% getByIndex(1)
  expect_equal(depot@name, "DEPOT")
  
  expect_error(model@compartments %>% getByIndex(5), regexp="Can't find element at index 5")
})
