
library(testthat)

context("Test the conversion to RxODE")

testFolder <<- ""
overwriteNonRegressionFiles <<- FALSE

source(paste0(testFolder, "testUtils.R"))

test_that("Export method works", {
  regFilename = "2_cpt_model_abs_comments"
  model <- getNONMEMModelTemplate(4,4)
  
  # Add some comments/line breaks programmatically
  main <- model@model %>% getByName("MAIN")
  main <- main %>% replace(Equation("KA", "THETA_KA*exp(ETA_KA)", comment="EQUATION COMMENT"))
  main <- main %>% add(LineBreak())
  main <- main %>% add(Comment("THIS IS A COMMENT"))
  
  model <- model %>% replace(main)
  
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  model <- model %>% add(LagTime(compartment=2, rhs="2"))
  model <- model %>% add(InfusionDuration(compartment=2, rhs="2"))
  model <- model %>% add(InfusionRate(compartment=1, rhs="2"))
  model <- model %>% add(InitialCondition(compartment=2, rhs="50"))
  
  # Code comparison
  rxmod <- model %>% export(dest="RxODE")
  rxodeNonRegTest(rxmod, regFilename)
  
  # Theta vector comparison
  expect_equal(c(THETA_KA=1, THETA_CL=5, THETA_V2=80, THETA_V3=20, THETA_Q=4), rxmod@theta)
  
  # Omega matrix comparison
  omega <- diag(rep(0.025, 5))
  rownames(omega) <- c("ETA_KA", "ETA_CL", "ETA_V2", "ETA_V3", "ETA_Q")
  colnames(omega) <- c("ETA_KA", "ETA_CL", "ETA_V2", "ETA_V3", "ETA_Q")
  expect_equal(omega, rxmod@omega)
  
  # Sigma matrix comparison
  sigma <- matrix(0.025)
  rownames(sigma) <- c("EPS_PROP")
  colnames(sigma) <- c("EPS_PROP")
  expect_equal(sigma, rxmod@sigma)
})
