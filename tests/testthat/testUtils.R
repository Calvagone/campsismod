
# setwd("C:/prj/campsismod/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismod/tests/")
# testFolder <- "C:/prj/campsismod/tests/testthat/"
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
