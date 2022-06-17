
library(testthat)

context("Test the conversion to mrgsolve")

source(paste0("", "testUtils.R"))

test_that("Export method works (2-cpt model, comments)", {
  regFilename = "2_cpt_model_abs_comments"
  model <- model_suite$nonmem$advan4_trans4
  
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
  
  mrgmod <- model %>% export(dest="mrgsolve")
  mrgsolveNonRegTest(mrgmod, regFilename)
})

test_that("Export method works (2-cpt model, if-statements)", {
  regFilename = "2_cpt_model_abs_if_statements"
  
  model <- model_suite$nonmem$advan4_trans4
  model <- model %>% delete(Equation("KA"))
  model <- model %>% add(Equation("KA", "0"))
  model <- model %>% add(IfStatement("OCC==1", Equation("KA", "THETA_KA*1.5*exp(ETA_KA)")))
  model <- model %>% add(IfStatement("OCC==2", Equation("KA", "THETA_KA*0.5*exp(ETA_KA)")))
  model <- model %>% add(IfStatement("OCC==3", Equation("KA", "THETA_KA*0.1*exp(ETA_KA)")))
  mrgmod <- model %>% export(dest="mrgsolve")
  mrgsolveNonRegTest(mrgmod, regFilename)
})

test_that("No table block exported if empty error block", {
  model <- model_suite$nonmem$advan4_trans4
  model <- model %>% delete(ErrorRecord())  
  mrgmod <- model %>% export(dest="mrgsolve")
  expect_equal(mrgmod@table, character(0))
})
