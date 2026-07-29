
# setwd("C:/prj/campsismod/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismod/tests/")
# TEST_FOLDER <<- "C:/prj/campsismod/tests/testthat/"

TEST_FOLDER <- ""
OVERWRITE_NON_REG_FILES <- FALSE

to_file <- function(code, path) {
  fileConn <- file(path)
  writeLines(code, fileConn)
  close(fileConn)
}

campsis_non_reg_path <- function(regFilename) {
  return(paste0(TEST_FOLDER, "non_regression/campsis/", regFilename))
}

mrgsolve_non_reg_path <- function(regFilename) {
  return(paste0(TEST_FOLDER, "non_regression/mrgsolve/", regFilename, ".txt"))
}

rxode_non_reg_path <- function(regFilename) {
  return(paste0(TEST_FOLDER, "non_regression/rxode/", regFilename, ".txt"))
}

campsis_non_reg_test <- function(model, regFilename) {
  if (OVERWRITE_NON_REG_FILES) {
    model %>% write(file=campsis_non_reg_path(regFilename))
  }
  expectedModel <- read.campsis(file=campsis_non_reg_path(regFilename))
  expect_equal(model, expectedModel)
}

mrgsolve_non_reg_test <- function(mrgmod, regFilename) {
  mrgmodCode <- mrgmod %>% to_string()
  if (OVERWRITE_NON_REG_FILES) {
    to_file(mrgmodCode, mrgsolve_non_reg_path(regFilename))
  }
  expectedMrgmodCode <- readLines(con=mrgsolve_non_reg_path(regFilename)) %>% paste0(collapse="\n")
  expect_equal(mrgmodCode, expectedMrgmodCode)
}

rxode_non_reg_test <- function(rxmod, regFilename) {
  rxmodCode <- rxmod@code %>% paste0(collapse="\n")
  if (OVERWRITE_NON_REG_FILES) {
    to_file(rxmodCode, rxode_non_reg_path(regFilename))
  }
  expectedRxmodCode <- readLines(con=rxode_non_reg_path(regFilename)) %>% paste0(collapse="\n")
  expect_equal(rxmodCode, expectedRxmodCode)
}

read_campsis_model_no_params <- function(file) {
  # Only first warning is actually checked
  model <- expect_warning(read.campsis(file),
                          regexp="No file '(theta|omega|sigma)\\.csv' could be found")
  return(model)
}

on_cran <- function() {
  # Copied from testthat:::on_cran() 
  return(!interactive() && !env_var_is_true("NOT_CRAN"))
}

env_var_is_true <- function(x) {
  return(isTRUE(as.logical(Sys.getenv(x, "false"))))
}

skip_performance_tests <- function() {
  # On CRAN, default value is TRUE
  # FALSE otherwise
  return(get_campsismod_option(name="SKIP_PERFORMANCE_TESTS", default=on_cran()))
}

