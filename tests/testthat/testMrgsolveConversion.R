
library(testthat)

context("Test the conversion to mrgsolve")

test_that("Export method works", {
  model <- getNONMEMModelTemplate(4,4)
  
  # Add some comments/line breaks programmatically
  main <- model@model %>% getByName("MAIN")
  main@code[1] <- paste(main@code[1], "# EQUATION COMMENT")
  main@code <- main@code %>% append("")
  main@code <- main@code %>% append("# THIS IS A COMMENT")
  model <- model %>% replace(main)
  
  model <- model %>% add(Bioavailability(compartment=1, rhs="0.75"))
  model <- model %>% add(LagTime(compartment=2, rhs="2"))
  model <- model %>% add(InfusionDuration(compartment=2, rhs="2"))
  model <- model %>% add(InfusionRate(compartment=1, rhs="2"))
  model <- model %>% add(InitialCondition(compartment=2, rhs="50"))
  
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
      "double KA=THETA_KA*exp(ETA_KA); // EQUATION COMMENT",
      "double CL=THETA_CL*exp(ETA_CL);",
      "double V2=THETA_V2*exp(ETA_V2);",
      "double V3=THETA_V3*exp(ETA_V3);",
      "double Q=THETA_Q*exp(ETA_Q);",
      "double S2=V2;",
      "", # Line break
      "// THIS IS A COMMENT",
      "F_A_DEPOT=0.75;",
      "ALAG_A_CENTRAL=2;",
      "D_A_CENTRAL=2;",
      "R_A_DEPOT=2;",
      "A_CENTRAL_0=50;"
    )
  expect_equal(mrgsolve@main[-1], main)
})

test_that("ToString method works", {
  model <- getNONMEMModelTemplate(4,4)
  mrgsolve <- model %>% export(dest="mrgsolve")
  cppFileContent <- mrgsolve %>% toString()
  expect_equal(nchar(cppFileContent), 928) # Just to not have an empty test...
})

test_that("appendComma is working well", {
  expect_equal(appendComma("KA=THETA_KA*exp(ETA_KA) # Comment"), "KA=THETA_KA*exp(ETA_KA); # Comment")
  expect_equal(appendComma("KA=THETA_KA*exp(ETA_KA)# Comment"), "KA=THETA_KA*exp(ETA_KA);# Comment")
  expect_equal(appendComma("KA=THETA_KA*exp(ETA_KA)   # Comment"), "KA=THETA_KA*exp(ETA_KA);   # Comment")
})