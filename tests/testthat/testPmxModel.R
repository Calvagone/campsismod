
library(testthat)

context("Test PMX model")

test_that("getCompartmentIndex method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  index_depot <- model %>% getCompartmentIndex("DEPOT")
  index_central <- model %>% getCompartmentIndex("CENTRAL")
  
  expect_equal(index_depot, 1)
  expect_equal(index_central, 2)
  expect_error(model %>% getCompartmentIndex("XX"))
})
