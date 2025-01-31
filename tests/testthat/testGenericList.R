library(testthat)

context("Test the generic list")

source(paste0("", "testUtils.R"))

test_that("GetByIndex method works well", {
  model <- model_suite$testing$nonmem$advan4_trans4
  
  depot <- model@compartments %>% getByIndex(1)
  expect_equal(depot@name, "DEPOT")
  
  expect_error(model@compartments %>% getByIndex(5), regexp="Can't find element at index 5")
})

test_that("Generic methods should throw an error when the call is incorrect", {
  msg <- "No default function is provided"
  expect_error(getPrefix(""), regexp=msg)
  expect_error(getRecordName(""), regexp=msg)
  expect_error(replace(""), regexp=msg)
  expect_error(indexOf(""), regexp=msg)
  expect_error(getByName(""), regexp=msg)
  expect_error(contains(""), regexp=msg)
  expect_error(find(""), regexp=msg)
  expect_error(getNames(""), regexp=msg)
  expect_error(getByIndex (""), regexp=msg)
  expect_error(sort(""), regexp=msg)
  expect_error(default(""), regexp=msg)
})

