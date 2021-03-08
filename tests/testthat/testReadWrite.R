
library(testthat)

context("Test read/write methods on PMX model")

testFolder <<- ""
testFolder <<- "C:/prj/pmxmod/tests/testthat/"

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

writePath <- function(advan, trans) {
  return(paste0(testFolder, "write/models/", advanFilename(advan, trans, ext="")))
}

generateModel <- function(advan, trans) {
  pmxtran <- pmxtran::importNONMEM(pmxtran::getNONMEMModelTemplate(advan, trans))
  pmxmod <- pmxtran::toPmxModel(pmxtran)
  return(pmxmod)
}

test_that("Write/Read ADVAN1 TRANS1", {
  advan <- 1
  trans <- 1
  pmxmod <- generateModel(advan, trans)
  
  # write
  pmxmod %>% write(file=writePath(advan, trans), zip=FALSE)
  
  # read
  pmxmod2 <- read.pmxmod(file=writePath(advan, trans))

  # Check equality  
  expect_equal(pmxmod@model, pmxmod2@model)
  expect_equal(pmxmod@parameters, pmxmod2@parameters)
})
