
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

test_that("Invalid OMEGA parameters raise an error", {
  expect_error(Omega(name="CL_V", index=1, index2=2, value=0.25, type="corr"),
               regexp="Type should be one of: 'var', 'sd', 'covar', 'cor', 'cv' or 'cv%'")
  
  expect_error(Omega(name="CL_V", index=1, index2=2, value=0.25, type="var"),
               regexp="Parameter type must be 'covar' or 'cor' \\(index:1, index2:2\\)")
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
  theta1 <- Theta(name="CL", index=1, min=0, max=Inf)
  df <- theta1 %>% as.data.frame(row.names=character(), optional=F)
  
  expect_equal(data.frame(name="CL", index=1, value=as.numeric(NA), min=as.numeric(0), max=as.numeric(Inf), fix=FALSE,
                          label=as.character(NA), unit=as.character(NA), comment=as.character(NA)), df)
})

test_that("Standardise method works as expected", {
  
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
  
  # Co-variance (covar)
  omega <- Omega(index=1, index2=2, value=0.2, type="covar")
  omega_ <- omega %>% standardise()
  expect_equal(omega, omega_) # No change
  
  # Correlation (cor)
  omega <- Omega(index=1, index2=2, value=0.25, type="cor")
  expect_error(omega %>% standardise(), regexp="Argument 'parameters' is needed to convert a covariance into a correlation")
})

test_that("Malformed omega is not valid and can't be standardised", {
  omega1 <- Omega(index=1, index2=2, value=0, type="covar")
  
  # Force type to be unknown
  omega1@type <- "unknown"
  
  # Check omega is not valid
  expect_error(validObject(omega1), regexp="Parameter type must be 'covar' or 'cor'")
  
  # Check omega can't be standardised
  expect_error(omega1 %>% standardise(), regexp="Type of parameter OMEGA_1_2 must be 'covar' or 'cor'")
  
  omega2 <- Omega(index=2, index2=2, value=0, type="var")
  
  # Force type to be unknown
  omega2@type <- "unknown"
  
  # Check omega is not valid
  expect_error(validObject(omega2), regexp="Type should be one of: 'var', 'sd', 'covar', 'cor', 'cv' or 'cv%'")
  
  # Check omega can't be standardised
  expect_error(omega2 %>% standardise(), regexp="Type should be one of: 'var', 'sd', 'cv' or 'cv%'")
  
  # Force type to be covar
  omega2@type <- "covar"
  
  # Check omega can't be standardised
  expect_error(omega2 %>% standardise(), regexp="Type of parameter OMEGA_2_2 can't be 'covar'")
})
