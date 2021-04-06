
library(testthat)

context("Test the conversion to mrgsolve")

test_that("Export method works", {
  model <- getNONMEMModelTemplate(4,4)
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  model <- model %>% add(LagTime(compartment=2, rhs="2"))
  model <- model %>% add(InfusionDuration(compartment=2, rhs="2"))
  mrgsolve <- model %>% export(dest="mrgsolve")
  param <-
    c(
      "THETA_KA : 1 : THETA_KA",
      "THETA_CL : 5 : THETA_CL",
      "THETA_V2 : 80 : THETA_V2",
      "THETA_V3 : 20 : THETA_V3",
      "THETA_Q : 4 : THETA_Q"
    )
  expect_equal(mrgsolve@param[-1], param)
  
  cmt <-
    c(
      "A_DEPOT : DEPOT",
      "A_CENTRAL : CENTRAL",
      "A_PERIPHERAL : PERIPHERAL",
      "A_OUTPUT : OUTPUT"
    )
  expect_equal(mrgsolve@cmt[-1], cmt)
  
  main <-
    c(
      "double KA=THETA_KA*exp(ETA_KA);",
      "double CL=THETA_CL*exp(ETA_CL);",
      "double V2=THETA_V2*exp(ETA_V2);",
      "double V3=THETA_V3*exp(ETA_V3);",
      "double Q=THETA_Q*exp(ETA_Q);",
      "double S2=V2;",
      "F_A_DEPOT=0.75;",
      "ALAG_A_CENTRAL=2;",
      "D_A_CENTRAL=2;"
    )
  expect_equal(mrgsolve@main[-1], main)
})

test_that("ToString method works", {
  model <- getNONMEMModelTemplate(4,4)
  mrgsolve <- model %>% export(dest="mrgsolve")
  cppFileContent <- mrgsolve %>% toString()
  expect_equal(nchar(cppFileContent), 927) # Just to not have an empty test...
})