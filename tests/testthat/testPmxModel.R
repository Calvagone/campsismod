
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

test_that("add method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  
  # Add a parameter
  model <- model %>% add(Theta(name="XX", index=6, value=1))
  expect_equal(model@parameters %>% select("theta") %>% length(), 6)
  
  # Add a code record
  # TODO: update this
  # model <- model %>% add(PredRecord())
  # model@model <- model@model %>% sort()
  # expect_equal(model@model %>% length(), 4)
})

test_that("replace method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  
  # Replace a parameter
  model <- model %>% replace(Theta(name="KA", index=1, value=1.5))
  expect_equal((model@parameters %>% getByName("THETA_KA"))@value, 1.5)
  
  # Replace a code record
  error <- ErrorRecord()
  model <- model %>% replace(error)
  expect_equal((model@model %>% getByName("ERROR"))@code %>% length(), 0)
})

