
library(testthat)

context("Test read/write methods on PMX model")

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
  model2 <- read.pmxmod(file=writePath(advan, trans))

  # Check equality  
  expect_equal(model, model2)
})

test_that("Write/Read ADVAN4 TRANS4, add compartment characteristics", {
  advan <- 4
  trans <- 4
  model <- getNONMEMModelTemplate(advan, trans)
  
  # Add a few characteristics
  model <- model %>% add(LagTime(1, "ALAG1"))
  model <- model %>% add(Bioavailability(1, "F1"))
  model <- model %>% add(InfusionDuration(1, "D1"))
  model <- model %>% add(InfusionDuration(2, "R2", rate=TRUE))
  
  # Write
  model %>% write(file=writePath(advan, trans))
  
  # Read
  model2 <- read.pmxmod(file=writePath(advan, trans))
  
  # Check equality  
  expect_equal(model, model2)
})
