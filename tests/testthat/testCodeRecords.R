
library(testthat)

context("Test all methods from the code records class")

testFolder <<- ""

test_that("Check records can be added correctly into the CAMPSIS model", {
  
  model <- CampsisModel()
  model <- model %>% add(MainRecord())
  model <- model %>% add(ErrorRecord())
  
  expect_true(!is.null(model %>% find(MainRecord())))
  expect_true(!is.null(model %>% find(ErrorRecord())))
  expect_true(is.null(model %>% find(OdeRecord())))
})

test_that("Check non-record items can't be added to a list of code records", {
  records <- CodeRecords()
  expect_error(records %>% add(Theta(index=1)), regexp="Element 'THETA_1' does not extend type 'code_record'")
})

test_that("Write/Read methods", {
  
  model1 <- CodeRecords()
  model1 <- model1 %>% add(MainRecord(code=c("A=1", "B=2")))
  model1 <- model1 %>% add(ErrorRecord(code=c("C=3")))
  
  # Write model
  # Warning is well provided: model not provided, compartment characteristics will be lost
  expect_warning(model1 %>% write(file=paste0(testFolder, "write/records/records1.mod")))
  
  # Read model
  model2 <- read.model(file=paste0(testFolder, "write/records/records1.mod"))
  
  expect_equal(model1, model2)
})

test_that("Sort methods", {
  
  model <- CampsisModel()
  model <- model %>% add(OdeRecord())
  model <- model %>% add(ErrorRecord())
  model <- model %>% add(MainRecord())
  
  expectedModel <- CampsisModel()
  expectedModel <- expectedModel %>% add(MainRecord())
  expectedModel <- expectedModel %>% add(OdeRecord())
  expectedModel <- expectedModel %>% add(ErrorRecord())
  
  expect_equal(model %>% sort(), expectedModel)
})

test_that("Create very basic model on the fly", {
  model <- CampsisModel()
  model <- model %>% add(Ode("A_CENTRAL", "-K*A_CENTRAL"))
  model <- model %>% add(Equation("THALF", "12"))
  
  expect_equal(model@model %>% getNames(), c("MAIN", "ODE"))
  
  # Check THALF can be found in MAIN code record
  expect_equal(model %>% find(MainRecord()) %>% find(Equation("THALF")), Equation("THALF", "12"))
  
  # Check A_CENTRAL ODE can be found in ODE record
  expect_equal(model %>% find(OdeRecord()) %>% find(Ode("A_CENTRAL")), Ode("A_CENTRAL", "-K*A_CENTRAL"))    
})

test_that("Equations can be removed", {
  
  model <- model_library$advan1_trans1
  expect_equal(model %>% find(MainRecord()) %>% length(), 3) # 3 equations: K, V, S1
  
  model <- model %>% delete(Equation("S1"))
  expect_equal(model %>% find(MainRecord()) %>% length(), 2) # 2 equations: K, V
})

test_that("Equations can be replaced", {
  
  model <- model_library$advan1_trans1
  expect_equal(model %>% find(MainRecord()) %>% length(), 3) # 3 equations: K, V, S1
  
  model <- model %>% replace(Equation("S1", "V/1000"))
  equation <- model %>% find(Equation("S1"))
  expect_equal(equation@rhs, "V/1000") # Equation well modified
})

test_that("Equations can be added at a specific position", {
  
  model <- model_library$advan1_trans1
  
  model1 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)"), Position(Equation("V")))
  model2 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)"), Position(Equation("S1"), after=FALSE))
  
  expect_equal(model1, model2)
  
  model3 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)"), Position(Equation("S1")))
  model4 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)")) # Will be appended to MAIN block
  
  expect_equal(model3, model4)
  
  # By index
  model5 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)"), Position(2))
  model6 <- model %>% add(Equation("V2", "THETA_V2*exp(ETA_V2)"), Position(3, after=FALSE))
  
  expect_equal(model5, model6)
})

test_that("Find method works well to search for a specific equation", {
  
  model <- model_library$advan1_trans1
  equation <- model %>% find(Equation("V"))
  expect_equal(equation@rhs, "THETA_V*exp(ETA_V)")
  
  equation <- model %>% find(Equation("V2"))
  expect_true(is.null(equation))
})

test_that("Contains method can be used to check the existence of an equation", {
  
  model <- model_library$advan1_trans1
  expect_true(model %>% contains(Equation("V")))
  expect_false(model %>% contains(Equation("V2")))
})

test_that("Add IF-statements at specific locations in the model", {
  
  model <- model_library$advan1_trans1
  
  if1 <- IfStatement("COV==1", Equation("V", "V*1.0"))
  if2 <- IfStatement("COV==2", Equation("V", "V*1.1"))
  if3 <- IfStatement("COV==3", Equation("V", "V*1.2"))
  
  # Add before Equation S1
  model <- model %>% add(if1, Position(Equation("S1"), after=F))
  model <- model %>% add(if2, Position(Equation("S1"), after=F))
  
  # Or add after previous IF statement
  model <- model %>% add(if3, Position(if2, after=T))

  main <- model %>% find(MainRecord())
  expect_equal(main@statements %>% getByIndex(3), if1)
  expect_equal(main@statements %>% getByIndex(4), if2)
  expect_equal(main@statements %>% getByIndex(5), if3)
  expect_equal(main@statements %>% getByIndex(6), Equation("S1", "V"))
})



