
library(testthat)

context("Test read/write methods on CAMPSIS model")

testFolder <<- ""

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

writePath <- function(advan, trans) {
  return(paste0(testFolder, "write/models/", advanFilename(advan, trans, ext="")))
}

test_that("Write/Read ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  model <- getNONMEMModelTemplate(advan, trans)
  
  # write
  model %>% write(file=writePath(advan, trans))
  
  # read
  model2 <- read.campsis(file=writePath(advan, trans))

  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN4 TRANS4, add compartment characteristics, add initial conditions", {
  advan <- 4
  trans <- 4
  model <- getNONMEMModelTemplate(advan, trans)
  
  # Add a few properties
  model <- model %>% add(LagTime(1, "ALAG1"))
  model <- model %>% add(Bioavailability(1, "F1"))
  model <- model %>% add(InfusionDuration(1, "D1"))
  model <- model %>% add(InfusionRate(2, "R2"))
  model <- model %>% add(InitialCondition(2, "100"))
  
  # Sort properties
  model <- model %>% sort()
  
  # Write
  model %>% write(file=writePath(advan, trans))
  prop <- model@compartments@properties
  # Read
  model2 <- read.campsis(file=writePath(advan, trans))
  prop2 <- model2@compartments@properties
  
  # Check equality  
  expect_equal(model, model2)
})

test_that("Parsing non standard record delimiters works as expected", {
  model <- expect_warning(read.campsis(paste0(testFolder, "custom/", "non_standard_record_delimiters/")),
                          regexp="No file '(theta|omega|sigma)\\.csv' could be found") # Only first warning is checked
  expect_equal(model %>% find(MainRecord()) %>% length(), 3)
  expect_equal(model %>% find(OdeRecord()) %>% length(), 3)
  expect_equal(model %>% find(ErrorRecord()) %>% length(), 1)
})

test_that("Parsing unknown record delimiters must raise an error", {
  expect_error(read.campsis(paste0(testFolder, "custom/", "unknow_record_delimiters/")),
               regexp="Record delimiter 'PK' is unknown")
})

test_that("Record delimiters may accept comments", {
  model <- expect_warning(read.campsis(paste0(testFolder, "custom/", "record_delimiters_with_comment/")),
                          regexp="No file '(theta|omega|sigma)\\.csv' could be found") # Only first warning is checked

  expect_equal(model %>% find(MainRecord()) %>% .@comment, "MAIN block")
  expect_equal(model %>% find(OdeRecord()) %>% .@comment, "ODE block")
  expect_equal(model %>% find(ErrorRecord()) %>% .@comment, "ERROR block")
  
  expect_true("[MAIN] # MAIN block" %in% capture.output(show(model)))
  expect_true("[ODE] # ODE block" %in% capture.output(show(model)))
  expect_true("[ERROR] # ERROR block" %in% capture.output(show(model)))
  
  # Please note that comments will be LOST for all properties record delimiters
  # This is because compartment properties are 'modeled' all-together in model@compartments@properties  
})


