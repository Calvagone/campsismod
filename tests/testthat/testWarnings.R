
library(testthat)

context("Check warnings are displayed in some particular cases")

testFolder <<- ""

source(paste0(testFolder, "testUtils.R"))

test_that("Warnings displayed when model contains unknown statements", {
  modelName <- "unknown_statement_warning"
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="Model code contains unknown statements")
})

test_that("Warnings displayed when no theta, omega and sigma.csv files", {
  modelName <- "unknown_statement_warning"
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'theta.csv' could be found")
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'omega.csv' could be found")
  expect_warning(read.campsis(paste0(testFolder, "custom/", modelName)), regexp="No file 'sigma.csv' could be found")
})