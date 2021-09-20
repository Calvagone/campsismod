
library(testthat)

context("Check warnings are displayed in some particular cases")

testFolder <<- ""

source(paste0(testFolder, "testUtils.R"))

test_that("Warnings displayed when model contains unknown statements (1)", {
  modelName <- "unknown_statement_hello"
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="Model code contains unknown statements")
})

test_that("Warnings displayed when model contains unknown statements (2)", {
  modelName <- "unknown_statement_illegal_variable_name"
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="Model code contains unknown statements")
})

test_that("Warnings displayed when no theta, omega and sigma.csv files", {
  modelName <- "unknown_statement_hello"
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'theta.csv' could be found")
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'omega.csv' could be found")
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'sigma.csv' could be found")
  
  model <- suppressWarnings(read.campsis(paste0(testFolder, "custom/", modelName)))
  mrgmod <- model %>% export(dest="mrgsolve")
  expect_equal(mrgmod@param, "[PARAM] @annotated")
  expect_equal(mrgmod@omega, character(0))
  expect_equal(mrgmod@sigma, character(0))
})

test_that("Error displayed if model file has ODE's in non ODE record", {
  modelName <- "ode_in_non_ode_record"
  expect_error(suppressWarnings(read.campsis(paste0(testFolder, "custom/", modelName))), regexp="ODE detected in non ODE record")
})

test_that("Error displayed if model file has an IF-statement in a properties record", {
  modelName <- "if_in_properties_record"
  expect_error(suppressWarnings(read.campsis(paste0(testFolder, "custom/", modelName))), regexp="IF-statement detected in properties record")
})