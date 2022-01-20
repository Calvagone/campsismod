
# setwd("C:/prj/campsismod/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismod/tests/")
# testFolder <<- "C:/prj/campsismod/tests/testthat/"
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# reticulate::py_config()
# version <- pharmpy["__version__"]

toFile <- function(code, path) {
  fileConn <- file(path)
  writeLines(code, fileConn)
  close(fileConn)
}

campsisNonRegPath <- function(regFilename) {
  return(paste0(testFolder, "non_regression/campsis/", regFilename))
}

mrgsolveNonRegPath <- function(regFilename) {
  return(paste0(testFolder, "non_regression/mrgsolve/", regFilename, ".txt"))
}

rxodeNonRegPath <- function(regFilename) {
  return(paste0(testFolder, "non_regression/rxode/", regFilename, ".txt"))
}

campsisNonRegTest <- function(model, regFilename) {
  if (overwriteNonRegressionFiles) {
    model %>% write(file=campsisNonRegPath(regFilename))
  }
  expectedModel <- read.campsis(file=campsisNonRegPath(regFilename))
  expect_equal(model, expectedModel)
}

mrgsolveNonRegTest <- function(mrgmod, regFilename) {
  mrgmodCode <- mrgmod %>% toString()
  if (overwriteNonRegressionFiles) {
    toFile(mrgmodCode, mrgsolveNonRegPath(regFilename))
  }
  expectedMrgmodCode <- readLines(con=mrgsolveNonRegPath(regFilename)) %>% paste0(collapse="\n")
  expect_equal(mrgmodCode, expectedMrgmodCode)
}

rxodeNonRegTest <- function(rxmod, regFilename) {
  rxmodCode <- rxmod@code %>% paste0(collapse="\n")
  if (overwriteNonRegressionFiles) {
    toFile(rxmodCode, rxodeNonRegPath(regFilename))
  }
  expectedRxmodCode <- readLines(con=rxodeNonRegPath(regFilename)) %>% paste0(collapse="\n")
  expect_equal(rxmodCode, expectedRxmodCode)
}

readCampsisModelNoParams <- function(file) {
  # Only first warning is actually checked
  model <- expect_warning(read.campsis(file),
                          regexp="No file '(theta|omega|sigma)\\.csv' could be found")
  return(model)
}
