
library(testthat)

context("Test read/write methods on PMX model")

testFolder <<- ""
testFolder <<- "C:/prj/pmxmod/tests/testthat/"

advanFilename <- function(advan, trans, ext=".txt") {
  return(paste0("advan", advan, "_trans", trans, ext))
}

modelPath <- function(advan, trans) {
  return(paste0(testFolder, "models/subroutine/", advanFilename(advan, trans, ext=".mod")))
}

writePath <- function(advan, trans) {
  return(paste0(testFolder, "write/models/", advanFilename(advan, trans, ext="")))
}

generateModel <- function(advan, trans) {
  pmxtran <- pmxtran::importNONMEM(modelPath(advan, trans))
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
  pmxmod2 <- read(file=writePath(advan, trans))

  # Check equality  
  expect_equal(pmxmod@code, pmxmod2@code)
  expect_equal(pmxmod@parameters, pmxmod2@parameters)
})
