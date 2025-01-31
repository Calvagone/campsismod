
library(testthat)

context("Test all methods that may be applied on compartments")

test_that("Method 'add' can't be used to add a compartment (add an ODE instead)", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_error(model %>% add(Compartment(index=model@compartments %>% length() + 1, name="OUTPUT2")),
               regexp="No default function is provided")
})

test_that("Method 'contains' works with compartments", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_true(model %>% contains(Compartment(1)))
  expect_false(model %>% contains(Compartment(5)))
})

test_that("Method 'delete' can't be used to delete a compartment (at least for now)", {
  model <- model_suite$testing$nonmem$advan4_trans4
  expect_error(model %>% delete(Compartment(index=1)), regexp="No default function is provided")
})

test_that("Method 'find' may return a compartment", {
  # Example 1
  model <- model_suite$testing$nonmem$advan4_trans4
  compartment <- model %>% find(Compartment(index=2))
  
  expect_equal(compartment@index, 2)
  expect_equal(compartment@name, "CENTRAL")
  
  # Example 2
  model <- model_suite$testing$nonmem$advan1_trans1
  compartments <- model@compartments
  compartment1 <- compartments %>% find(Compartment(index=1))
  compartment2 <- compartments %>% find(Compartment(index=2))
  
  expect_equal(compartments %>% length(), 2)
  expect_equal(compartment1@name, "CENTRAL")
  expect_equal(compartment2@name, "OUTPUT")
  
  expect_equal(capture_output(show(compartment1)), "A_CENTRAL (CMT=1)")
  expect_equal(capture_output(show(compartment2)), "A_OUTPUT (CMT=2)")
})

test_that("Method 'getCompartmentIndex' works as expected", {
  model <- model_suite$testing$nonmem$advan4_trans4
  index_depot <- model %>% getCompartmentIndex("DEPOT")
  index_central <- model %>% getCompartmentIndex("CENTRAL")
  
  expect_equal(index_depot, 1)
  expect_equal(index_central, 2)
  expect_error(model %>% getCompartmentIndex("XX"), regexp="Compartment XX not found")
})

test_that("Method 'replace' can be used to adapt the name of a compartment", {
  model <- model_suite$testing$nonmem$advan4_trans4 %>% add(Bioavailability(1, "0.75"))
  
  # Replace compartment and ODE
  model <- model %>% replace(Compartment(1, "ABS")) %>% 
    delete(Ode("A_DEPOT")) %>%
    add(Ode("A_ABS", "-KA*ABS"), Position(Ode("A_CENTRAL"), after=FALSE))
  
  # Replace occurrence in CENTRAL compartment
  central <- model %>% find(Ode("A_CENTRAL"))
  central@rhs <- gsub(pattern="A_DEPOT", replacement="A_ABS", x=central@rhs)
  model <- model %>% replace(central)
  
  # Check compartment has been correctly replaced
  compartment <- model %>% find(Compartment(1))
  expect_equal(compartment@name, "ABS")
  
  # Check 2 first ODE's
  abs <- model %>% find(Ode("A_ABS"))
  central <- model %>% find(Ode("A_CENTRAL"))
  expect_equal(abs@rhs, "-KA*ABS")
  expect_equal(central@rhs, "KA*A_ABS + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL")
  
  # Check bioavailability equation will be adapted too
  expect_equal(model %>% find(Bioavailability(1)) %>% toString(model=model, dest="campsis"), "A_ABS=0.75")
})

test_that("Method 'toString' works as expected on compartment", {
  cmt <- Compartment(index=1)
  expect_equal(cmt %>% toString(), "A_1")
  
  cmt <- Compartment(index=1, name="CENTRAL")
  expect_equal(cmt %>% toString(), "A_CENTRAL")
})
