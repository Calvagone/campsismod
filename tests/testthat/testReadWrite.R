
library(testthat)

context("Test read/write methods on CAMPSIS model")

source(paste0("", "testUtils.R"))

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

writePath <- function(modelName) {
  return(paste0(testFolder, "write/models/", modelName))
}

test_that("Write/Read ADVAN1 TRANS1", {
  modelName <- "advan1_trans1"
  model <- model_suite$nonmem[[modelName]]
  
  # write
  model %>% write(file=writePath(modelName))
  
  # read
  model2 <- read.campsis(file=writePath(modelName))

  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN3 TRANS4 with variance-covariance matrix", {
  modelName <- "my_model1"
  model <- model_suite$other[[modelName]]
  
  # write
  model %>% write(file=writePath(modelName))
  
  # read
  model2 <- read.campsis(file=writePath(modelName))
  
  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN4 TRANS4 with various compartment properties", {
  modelName <- "advan4_trans4"
  model <- model_suite$nonmem[[modelName]]
  
  # Add a few properties
  model <- model %>% add(LagTime(1, "ALAG1"))
  model <- model %>% add(Bioavailability(1, "F1"))
  model <- model %>% add(InfusionDuration(1, "D1"))
  model <- model %>% add(InfusionRate(2, "R2"))
  model <- model %>% add(InitialCondition(2, "100"))
  
  # Sort properties
  model <- model %>% sort()
  
  # Write
  model %>% write(file=writePath(modelName))
  prop <- model@compartments@properties
  # Read
  model2 <- read.campsis(file=writePath(modelName))
  prop2 <- model2@compartments@properties
  
  # Check equality  
  expect_equal(model, model2)
})
