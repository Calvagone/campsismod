
library(testthat)

context("Test parsing of record delimiters")

source(paste0("", "testUtils.R"))

test_that("Parsing non standard record delimiters works as expected", {
  folder <- paste0(testFolder, "custom/", "non_standard_record_delimiters/")
  model <- readCampsisModelNoParams(folder)
  expect_equal(model %>% find(MainRecord()) %>% length(), 3)
  expect_equal(model %>% find(OdeRecord()) %>% length(), 3)
  expect_equal(model %>% find(ErrorRecord()) %>% length(), 1)
})

test_that("Parsing unknown record delimiters must raise an error", {
  folder <- paste0(testFolder, "custom/", "unknow_record_delimiters/")
  expect_error(read.campsis(folder), regexp="Record delimiter 'PK' is unknown")
})

test_that("Record delimiters may accept comments", {
  folder <- paste0(testFolder, "custom/", "record_delimiters_with_comment/")
  model <- readCampsisModelNoParams(folder)

  expect_equal(model %>% find(MainRecord()) %>% .@comment, "MAIN block")
  expect_equal(model %>% find(OdeRecord()) %>% .@comment, "ODE block")
  expect_equal(model %>% find(ErrorRecord()) %>% .@comment, "ERROR block")
  
  expect_true("[MAIN] # MAIN block" %in% capture.output(show(model)))
  expect_true("[ODE] # ODE block" %in% capture.output(show(model)))
  expect_true("[ERROR] # ERROR block" %in% capture.output(show(model)))
  
  # Please note that comments will be LOST for all properties record delimiters
  # This is because compartment properties are 'modeled' all-together in model@compartments@properties  
})

test_that("Malformed record delimiters throw an error", {
  folder <- paste0(testFolder, "custom/", "malformed_record_delimiter1/")
  expect_error(readCampsisModelNoParams(folder), regexp="Record delimiter '\\[MAIN\\] Hello' is not valid")
  
  folder <- paste0(testFolder, "custom/", "malformed_record_delimiter2/")
  expect_error(readCampsisModelNoParams(folder), regexp="Record delimiter '\\[ODE\\] d/dt\\(A_CENTRAL\\)=-K\\*A_CENTRAL' is not valid")
})

test_that("A record delimiter must always exist", {
  folder <- paste0(testFolder, "custom/", "no_record_delimiter_available1/")
  expect_error(readCampsisModelNoParams(folder), regexp="Missing record delimiter at beginning of model")
  
  folder <- paste0(testFolder, "custom/", "no_record_delimiter_available2/")
  expect_error(readCampsisModelNoParams(folder), regexp="No record delimiter found in model")
})

test_that("Line breaks can precede the first record delimiter", {
  folder <- paste0(testFolder, "custom/", "line_breaks_before_main/")
  model <- readCampsisModelNoParams(folder)
  expect_equal(model %>% find(MainRecord()) %>% length(), 2)
  expect_equal(model %>% find(OdeRecord()) %>% length(), 3)
})

test_that("Comments can precede the first record delimiter", {
  folder <- paste0(testFolder, "custom/", "comments_before_main/")
  model <- readCampsisModelNoParams(folder)
  expect_equal(model %>% find(MainRecord()) %>% length(), 2)
  expect_equal(model %>% find(OdeRecord()) %>% length(), 3)
})
