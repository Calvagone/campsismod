
library(testthat)

context("Test all methods from utilities.R")

test_that("trim method is working well", {
  expect_equal(trim("  hello  "), "hello")
})

test_that("isODE method is working well", {
  expect_true(isODE("d/dt(A_DEPOT)=-KA*A_DEPOT"))
  expect_true(isODE("d/dt(A_OUTPUT)=K*A_CENTRAL"))
})

test_that("isLagTime method is working well", {
  expect_true(isLagTime("lag(A_DEPOT)=ALAG1"))
  expect_true(isLagTime("lag (A_DEPOT)=ALAG1"))
  expect_true(isLagTime("lag (A_DEPOT) =ALAG1"))
  expect_true(isLagTime("  lag (A_DEPOT) =  ALAG1"))
  expect_false(isLagTime("  lage (A_DEPOT) =  ALAG1"))
})

test_that("isBioavailibility method is working well", {
  expect_true(isBioavailibility(" f(A_DEPOT)=F1"))
  expect_false(isBioavailibility("lag(A_DEPOT)=ALAG1"))
})

test_that("isInfusionDuration method is working well", {
  expect_true(isInfusionDuration("dur(A_DEPOT)=D1"))
  expect_false(isInfusionDuration("lag(A_DEPOT)=ALAG1"))
})

test_that("isRate method is working well", {
  expect_true(isRate("rate(A_DEPOT)=R1"))
  expect_false(isRate("lag(A_DEPOT)=ALAG1"))
})

test_that("extractTextBetweenBrackets method is working well", {
  expect_equal(extractTextBetweenBrackets("d/dt(A_DEPOT)=-KA*A_DEPOT"), "A_DEPOT")
  expect_error(extractTextBetweenBrackets(c("d/dt(A_DEPOT)=-KA*A_DEPOT", "X=1")))
})

test_that("isEquation is working well", {
  expect_true(isEquation("V3=THETA_V3*VDBW"))
  expect_false(isEquation("if (OCC == 1) VIS1=1"))
})
