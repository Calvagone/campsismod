
library(testthat)

context("Test the conversion to mrgsolve")

test_that("Export method works", {
  model <- getNONMEMModelTemplate(4,4)
  model <- model %>% add(CompartmentBioavailability(compartment=1, rhs="0.75"))
  model <- model %>% add(CompartmentLagTime(compartment=2, rhs="2"))
  model <- model %>% add(CompartmentInfusionDuration(compartment=2, rhs="2"))
  mrgsolve <- model %>% export(dest="mrgsolve")
  param <-
    c(
      "THETA_1 : 1 : THETA_1",
      "THETA_2 : 5 : THETA_2",
      "THETA_3 : 80 : THETA_3",
      "THETA_4 : 20 : THETA_4",
      "THETA_5 : 4 : THETA_5"
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
      "double KA=THETA_1*exp(ETA_1);",
      "double CL=THETA_2*exp(ETA_2);",
      "double V2=THETA_3*exp(ETA_3);",
      "double V3=THETA_4*exp(ETA_4);",
      "double Q=THETA_5*exp(ETA_5);",
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
  expect_equal(nchar(cppFileContent), 894) # Just to not have an empty test...
})