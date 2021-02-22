
library(testthat)

context("Test parameter S4 objects")

test_that("Working THETA parameter", {
  
  theta1 <- new("theta", name="THETA_CL", index=as.integer(1), suffix="CL", fix=TRUE, value=4)
  expect_equal(theta1@name, "THETA_CL")
  expect_equal(theta1@index, 1)
  expect_equal(theta1@suffix, "CL")
  expect_equal(theta1@fix, TRUE)
  expect_equal(theta1 %>% getName, "THETA_CL")
})

test_that("THETA parameter, NA test", {
  theta1 <- new("theta", name=as.character(NA), index=as.integer(1), suffix="CL", fix=TRUE, value=4)
  expect_equal(theta1@name, as.character(NA))
})


test_that("Check incorrect length of parameter", {
  expect_error(new("theta", name=c("THETA_CL", "THETA_CL"), index=as.integer(1), suffix="CL", fix=TRUE, value=4))
})

test_that("isDial method", {
  
  omega2_2 <- new("omega", name="OMEGA_CL", index=as.integer(2), index2=as.integer(2), suffix="CL", fix=TRUE, value=0.2)
  expect_true(isDiag(omega2_2))

  omega1_2 <- new("omega", name="OMEGA_CL", index=as.integer(1), index2=as.integer(2), suffix="CL", fix=TRUE, value=0.2)
  expect_false(isDiag(omega1_2))
})





