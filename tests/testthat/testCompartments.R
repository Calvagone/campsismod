
library(testthat)

context("Test all methods that may be applied on compartments")

test_that("add method can't be used to add a compartment (add an ODE instead)", {
  model <- model_library$advan4_trans4
  expect_error(model %>% add(Compartment(index=model@compartments %>% length() + 1, name="OUTPUT2")),
               regexp="No default function is provided")
})

test_that("contains method works with compartments", {
  model <- model_library$advan4_trans4
  expect_true(model %>% contains(Compartment(1)))
  expect_false(model %>% contains(Compartment(5)))
})

test_that("delete method can't be used to delete a compartment (at least for now)", {
  model <- model_library$advan4_trans4
  expect_error(model %>% delete(Compartment(index=1)), regexp="No default function is provided")
})

test_that("find method may return a compartment", {
  # Example 1
  model <- model_library$advan4_trans4
  compartment <- model %>% find(Compartment(index=2))
  
  expect_equal(compartment@index, 2)
  expect_equal(compartment@name, "CENTRAL")
  
  # Example 2
  model <- model_library$advan1_trans1
  compartments <- model@compartments
  compartment1 <- compartments %>% find(Compartment(index=1))
  compartment2 <- compartments %>% find(Compartment(index=2))
  
  expect_equal(compartments %>% length(), 2)
  expect_equal(compartment1@name, "CENTRAL")
  expect_equal(compartment2@name, "OUTPUT")
})

test_that("getCompartmentIndex method works as expected", {
  model <- model_library$advan4_trans4
  index_depot <- model %>% getCompartmentIndex("DEPOT")
  index_central <- model %>% getCompartmentIndex("CENTRAL")
  
  expect_equal(index_depot, 1)
  expect_equal(index_central, 2)
  expect_error(model %>% getCompartmentIndex("XX"), regexp="Compartment XX not found")
})

test_that("replace method may be used to adapt the name of a compartment", {
  model <- model_library$advan4_trans4 %>% add(Bioavailability(1, "0.75"))
  model <- model %>% replace(Compartment(1, "ABS"))
  
  # Check compartment has been correctly replaced
  compartment <- model %>% find(Compartment(1))
  expect_equal(compartment@name, "ABS")
  
  # Check bioavailability equation will be adapted too
  expect_equal(model %>% find(Bioavailability(1)) %>% toString(model=model, dest="campsis"), "A_ABS=0.75")
})
