
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

test_that("OMEGA index2 can't be NA", {
  # Missing index2 argument
  expect_error(Omega(index=1))
  
  # Missing index2 argument
  expect_error(Omega(index=1, index2=NA))
})

test_that("As.data.frame method", {
  theta1 <- Theta(name="CL", index=1)
  df <- theta1 %>% as.data.frame(row.names=character(), optional=F)
  
  expect_equal(data.frame(name="CL", index=1, value=as.numeric(NA), fix=FALSE), df)
})

test_that("Standardise method", {
  
  # Type not valid
  expect_error(Omega(index=1, index2=1, value=117, type="cv%%"))
  
  # Value in CV%
  omega <- Omega(index=1, index2=1, value=117, type="cv%")
  omega_ <- omega %>% standardise()
  expect_equal(round(omega_@value, 2), 0.86)
  
  # Value in CV
  omega <- Omega(index=1, index2=1, value=1.17, type="cv")
  omega_ <- omega %>% standardise()
  expect_equal(round(omega_@value, 2), 0.86)
  
  # Value in sd
  omega <- Omega(index=1, index2=1, value=0.2, type="sd")
  omega_ <- omega %>% standardise()
  expect_equal(omega_@value, 0.04)
})





