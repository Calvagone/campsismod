
library(testthat)

context("Test the conversion to mrgsolve")

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
  
  mrgmod <- model %>% export(dest="mrgsolve")
  mrgsolveNonRegTest(mrgmod, regFilename)
})

