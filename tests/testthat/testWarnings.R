
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
})