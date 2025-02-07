
library(testthat)

context("Test read/write methods on Campsis model")

source(paste0("", "testUtils.R"))

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

writePath <- function(modelName) {
  return(paste0(testFolder, "write/models/", modelName))
}

test_that("Write/Read ADVAN1 TRANS1", {
  modelName <- "advan1_trans1"
  model <- model_suite$testing$nonmem[[modelName]]
  
  # write
  model %>% write(file=writePath(modelName))
  
  # read
  model2 <- read.campsis(file=writePath(modelName))

  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN3 TRANS4 with variance-covariance matrix", {
  modelName <- "my_model1"
  model <- model_suite$testing$other[[modelName]]
  
  # write
  model %>% write(file=writePath(modelName))
  
  # read
  model2 <- read.campsis(file=writePath(modelName))
  
  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN4 TRANS4 with various compartment properties", {
  modelName <- "advan4_trans4"
  model <- model_suite$testing$nonmem[[modelName]]
  
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

test_that("Model parameters can be annotated and persisted (fields label, unit, comment)", {
  model <- model_suite$testing$pk$'1cpt_fo'
  modelName <- "model_1cpt_fo_annotated"
  
  model <- model %>%
    replace(Theta(name="KA", value=1, label="Absorption rate", unit="/h")) %>%
    replace(Omega(name="KA", value=25, type="cv%", label="IIV on absorption")) %>%
    replace(Sigma(name="RUV_FIX", value=1, fix=TRUE, label="Proportional error", comment="Fixed epsilon, multiplied by THETA_PROP_RUV in model code."))
  
  # Write
  model %>% write(file=writePath(modelName))
  
  # Read
  model2 <- read.campsis(file=writePath(modelName))
  
  # Check equality  
  expect_equal(model, model2)
  
})

test_that("Model parameters with min and max values can be persisted correctly", {
  model <- model_suite$testing$pk$'1cpt_fo'
  modelName <- "model_1cpt_fo_min_max"
  
  model <- model %>%
    setMinMax("theta", min=0, max=Inf) %>%
    setMinMax(Sigma("RUV_FIX"), min=1, max=1)
  
  # Write
  model %>% write(file=writePath(modelName))
  
  # Read
  model2 <- read.campsis(file=writePath(modelName))
  
  # Check equality  
  expect_equal(model, model2)
})
