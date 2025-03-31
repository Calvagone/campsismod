library(testthat)

context("Test various methods of the campsismod package that are not tested elsewhere")
source(paste0("", "testUtils.R"))

# options(campsismod.options=list(SKIP_PERFORMANCE_TESTS=TRUE))

test_that("Methods 'getName', 'getPrefix', 'show' and 'toString' work as expected on initial conditions", {
  model <- model_suite$testing$pk$`1cpt_fo`
  init <- InitialCondition(1, "1000")
  expect_equal(getName(init), "INIT (CMT=1)")
  expect_equal(getPrefix(init), "")
  expect_equal(capture_output(show(init)), "INIT (CMT=1): 1000")
  expect_equal(toString(init, dest="rxode2", model=model), "A_ABS(0)=1000")
  expect_equal(toString(init, dest="mrgsolve", model=model), "A_ABS_0=1000")
  expect_error(toString(init, dest="other", model=model))
})

test_that("Methods 'show' and 'toString' work as expected on compartment properties", {
  model <- model_suite$testing$pk$`1cpt_fo`
  property <- Bioavailability(1, rhs="F1")
  expect_equal(capture_output(show(property)), "BIOAVAILABILITY (CMT=1): F1")
  expect_equal(toString(property, dest="rxode2", model=model), "f(A_ABS)=F1")
  expect_equal(toString(property, dest="mrgsolve", model=model), "F_A_ABS=F1")
  expect_error(toString(property, dest="other", model=model))
})

test_that("Generic methods should throw an error when the call is incorrect", {
  msg <- "No default function is provided"
  expect_error(addRSE(""), regexp=msg)
  expect_error(autoDetectNONMEM(""), regexp=msg)
  expect_error(disable(""), regexp=msg)
  expect_error(export(""), regexp=msg)
  expect_error(getCompartmentIndex(""), regexp=msg)
  expect_error(getUncertainty(""), regexp=msg)
  expect_error(getVarCov(""), regexp=msg)
  expect_error(move(""), regexp=msg)
  expect_error(read(""), regexp=msg)
  expect_error(replaceAll(""), regexp=msg)
  expect_error(replicate("", ""), regexp=msg)
  expect_error(select (""), regexp=msg)
  expect_error(setMinMax("", "", "", ""), regexp=msg) # Strange, I need to pass the 4 args
  expect_error(standardise(""), regexp=msg)
  expect_error(toString (""), regexp=msg)
  expect_error(write(""), regexp=msg)
  
  expect_error(getPrefix(""), regexp=msg)
  expect_error(getRecordName(""), regexp=msg)
})

test_that("Method 'toString' of unknown statements works as expected", {
  statement <- UnknownStatement("HELLO")
  expect_equal(toString(statement, dest="campsis", show=TRUE), "[UNKNOWN STATEMENT] HELLO")
  expect_equal(toString(statement, dest="campsis", show=FALSE), "HELLO")
  expect_error(toString(statement, dest="other"), regexp="Only rxode2 \\(previously RxODE\\), mrgsolve or campsis are supported")
})

test_that("Export function on a replicated Campsis model should be fast", {
  model <- CampsisModel()
  noOfOmegas <- 11
  set.seed(123)
  replicates <- 1000
  
  # Add a huge OMEGA matrix
  for (index1 in 1:noOfOmegas) {
    for (index2 in 1:noOfOmegas) {
      if (index2 > index1) {
        next
      }
      model <- model %>%
        add(Omega(index=index1, index2=index2, value=ifelse(index1==index2, runif(1), 0)))
    }
  }
  
  repModel <- model %>%
    replicate(replicates, settings=AutoReplicationSettings(wishart=TRUE, odf=100, sdf=1000))
  
  noOfColumns <- noOfOmegas * (noOfOmegas - 1) / 2 + noOfOmegas # Lower triangle count + diagonal
  expect_equal(ncol(repModel@replicated_parameters) - 1, noOfColumns)
  
  # Check performances on exporting the Campsis model
  start <- Sys.time()
  models <- seq_len(replicates) %>%
    purrr::map(~repModel %>% export(dest=CampsisModel(), index=.x))
  end <- Sys.time()
  duration <- as.numeric(end - start)
  if (!skipPerformanceTests()) {
    expect_true(duration < 30) # 30 seconds (about 6 seconds on my machine)
  }
  
  
  # Check performances on exporting the OMEGA matrices
  start <- Sys.time()
  matrices <- models %>%
    purrr::map(~rxodeMatrix(.x))
  end <- Sys.time()
  duration <- as.numeric(end - start)
  
  if (!skipPerformanceTests()) {
    expect_true(duration < 15) # (about 3 seconds on my machine)
  }
  
  matrix1 <- matrices[[1]]
  expect_true(isSymmetric(matrix1))
  expect_equal(colnames(matrix1), paste0("ETA_", 1:noOfOmegas))
  
  # Check error message is given is OMEGA is missing
  model1 <- models[[1]] %>%
    delete(Omega(index=1, index2=1))
  expect_error(rxodeMatrix(model1), regexp="At least one OMEGA is missing")
})
