
library(testthat)

context("Test the conversion to RxODE")

test_that("Export method works", {
  model <- getNONMEMModelTemplate(4,4)
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  model <- model %>% add(LagTime(compartment=2, rhs="2"))
  model <- model %>% add(InfusionDuration(compartment=2, rhs="2"))
  model <- model %>% add(InfusionRate(compartment=1, rhs="2"))
  model <- model %>% add(InitialCondition(compartment=2, rhs="50"))
  
  rxode <- model %>% export(dest="RxODE")
  
  # Code comparison
  code <- "KA=THETA_KA*exp(ETA_KA)"
  code <- code %>% append("CL=THETA_CL*exp(ETA_CL)")
  code <- code %>% append("V2=THETA_V2*exp(ETA_V2)")
  code <- code %>% append("V3=THETA_V3*exp(ETA_V3)")
  code <- code %>% append("Q=THETA_Q*exp(ETA_Q)")
  code <- code %>% append("S2=V2")
  code <- code %>% append("d/dt(A_DEPOT)=-KA*A_DEPOT")
  code <- code %>% append("d/dt(A_CENTRAL)=KA*A_DEPOT + Q*A_PERIPHERAL/V3 + (-CL/V2 - Q/V2)*A_CENTRAL")
  code <- code %>% append("d/dt(A_PERIPHERAL)=-Q*A_PERIPHERAL/V3 + Q*A_CENTRAL/V2")
  code <- code %>% append("d/dt(A_OUTPUT)=CL*A_CENTRAL/V2")
  code <- code %>% append("F=A_CENTRAL/S2")
  code <- code %>% append("f(A_DEPOT)=0.75")
  code <- code %>% append("lag(A_CENTRAL)=2")
  code <- code %>% append("dur(A_CENTRAL)=2")
  code <- code %>% append("rate(A_DEPOT)=2")
  code <- code %>% append("A_CENTRAL(0)=50")
  code <- code %>% append("CP=F")
  code <- code %>% append("OBS_CP=CP*(EPS_PROP + 1)")
  code <- code %>% append("Y=OBS_CP")
  expect_equal(code, rxode@code)
  
  # Theta vector comparison
  expect_equal(c(THETA_KA=1, THETA_CL=5, THETA_V2=80, THETA_V3=20, THETA_Q=4), rxode@theta)
  
  # Omega matrix comparison
  omega <- diag(rep(0.025, 5))
  rownames(omega) <- c("ETA_KA", "ETA_CL", "ETA_V2", "ETA_V3", "ETA_Q")
  colnames(omega) <- c("ETA_KA", "ETA_CL", "ETA_V2", "ETA_V3", "ETA_Q")
  expect_equal(omega, rxode@omega)
  
  # Sigma matrix comparison
  sigma <- matrix(0.025)
  rownames(sigma) <- c("EPS_PROP")
  colnames(sigma) <- c("EPS_PROP")
  expect_equal(sigma, rxode@sigma)
})
