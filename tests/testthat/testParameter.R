
library(testthat)

context("Test parameter S4 objects")

test_that("Working THETA parameter", {
  
  theta1 <- Theta(name="CL", index=1, fix=TRUE, value=4)
  expect_equal(theta1@name, "CL")
  expect_equal(theta1@index, 1)
  expect_equal(theta1@fix, TRUE)
  expect_equal(theta1 %>% getName, "THETA_CL")
})

test_that("THETA parameter, default name", {
  theta1 <- Theta(index=1, fix=TRUE, value=4)
  expect_equal(theta1@name, as.character(NA))
})


test_that("Check incorrect length of parameter", {
  expect_error(Theta(name=c("CL", "CL"), index=1, fix=TRUE, value=4))
})

test_that("isDial method", {
  
  omega2_2 <- Omega(name="CL", index=2, index2=2, fix=TRUE, value=0.2)
  expect_true(isDiag(omega2_2))

  omega1_2 <- Omega(name="CL", index=1, index2=2, fix=TRUE, value=0.2)
  expect_false(isDiag(omega1_2))
})

test_that("getNONMEMName method", {
  
  theta1 <- Theta(name="CL", index=1)
  expect_equal(theta1 %>% getNONMEMName(), "THETA(1)")
  
  omega1_2 <- Omega(name="CL", index=1, index2=2)
  expect_equal(omega1_2 %>% getNONMEMName(), "OMEGA(1,2)")
  
  sigma1_2 <- Sigma(name="CL", index=1, index2=2)
  expect_equal(sigma1_2 %>% getNONMEMName(), "SIGMA(1,2)")
})





