library(testthat)

context("Test various methods of the campsismod package that are not tested elsewhere")

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
