library(testthat)

context("Test the auto-detect NONMEM function")

test_that("Bioavailability is well recognised", {
  model <- model_library$advan4_trans4
  model <- model %>% add(Equation("F1", "0.75"))
  model <- model %>% autoDetectNONMEM()
  property <- model %>% find(Bioavailability(1))
  expect_equal(property, Bioavailability(1, "F1"))
})

test_that("Infusion duration is well recognised", {
  model <- model_library$advan4_trans4
  model <- model %>% add(Equation("D2", "1"))
  model <- model %>% autoDetectNONMEM()
  property <- model %>% find(InfusionDuration(2))
  expect_equal(property, InfusionDuration(2, "D2"))
})

test_that("Infusion rate is well recognised", {
  model <- model_library$advan4_trans4
  model <- model %>% add(Equation("R2", "500"))
  model <- model %>% autoDetectNONMEM()
  property <- model %>% find(InfusionRate(2))
  expect_equal(property, InfusionRate(2, "R2"))
})

test_that("Infusion rate is well recognised", {
  model <- model_library$advan4_trans4
  model <- model %>% add(Equation("ALAG1", "2"))
  model <- model %>% autoDetectNONMEM()
  property <- model %>% find(LagTime(1))
  expect_equal(property, LagTime(1, "ALAG1"))
})