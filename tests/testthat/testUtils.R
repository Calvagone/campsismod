
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

loadNonRegressionFile <- function(path) {
  return(read.table(file=path, sep="@")[,1])
}

nonRegressionPath <- function(regFilename) {
  return(paste0(testFolder, "non_regression/", regFilename))
}

modelRegressionTest <- function(model, regFilename) {
  if (overwriteNonRegressionFiles) {
    model %>% write(file=nonRegressionPath(regFilename))
  }
  expectedModel <- read.pmxmod(file=nonRegressionPath(regFilename))
  expect_equal(model, expectedModel)
}