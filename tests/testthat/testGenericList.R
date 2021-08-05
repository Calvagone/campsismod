library(testthat)

context("Test the generic list")

testFolder <<- ""

test_that("GetByIndex method works well", {
  model <- model_library$advan4_trans4
  
  depot <- model@compartments %>% getByIndex(1)
  expect_equal(depot@name, "DEPOT")
  
  expect_error(model@compartments %>% getByIndex(5), regexp="Can't find element at index 5")
})