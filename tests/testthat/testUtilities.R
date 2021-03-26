
library(testthat)

context("Test all methods from utilities.R")

test_that("trim method is working well", {
  expect_equal(trim("  hello  "), "hello")
})

test_that("isODE method is working well", {
  expect_true(isODE("d/dt(A_DEPOT)=-KA*A_DEPOT"))
  expect_true(isODE("d/dt(A_OUTPUT)=K*A_CENTRAL"))
})

test_that("getODEName method is working well", {
  expect_equal(getODEName("d/dt(A_DEPOT)=-KA*A_DEPOT"), "A_DEPOT")
  expect_error(getODEName(c("d/dt(A_DEPOT)=-KA*A_DEPOT", "X=1")))
})

