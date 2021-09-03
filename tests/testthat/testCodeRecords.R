
library(testthat)

context("Test all methods from the code records class")

testFolder <<- ""

test_that("Add and get methods", {
  
  model <- CodeRecords()
  model <- model %>% add(MainRecord(code=""))
  model <- model %>% add(ErrorRecord(code=""))
  
  expect_true(model %>% getByName("MAIN") %>% length() > 0)
  expect_true(model %>% getByName("ERROR") %>% length() > 0)
  expect_true(model %>% getByName("ODE") %>% length() == 0)
})

test_that("Add not record objects", {
  model <- CodeRecords()
  expect_error(model %>% add(Theta(index=1))) # Element 'THETA_1' does not extend type 'code_record'.
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
  
  model <- CodeRecords()
  model <- model %>% add(OdeRecord())
  model <- model %>% add(ErrorRecord())
  model <- model %>% add(MainRecord())
  
  expectedModel <- CodeRecords()
  expectedModel <- expectedModel %>% add(MainRecord())
  expectedModel <- expectedModel %>% add(OdeRecord())
  expectedModel <- expectedModel %>% add(ErrorRecord())
  
  expect_equal(model %>% sort(), expectedModel)
})

test_that("Create very basic model on the fly", {
  
  records <- CodeRecords()
  records <- records %>% add(Ode("A_CENTRAL", "-K*A_CENTRAL"))
  records <- records %>% add(Equation("THALF", "12"))
  
  expect_equal(records %>% getNames(), c("MAIN", "ODE"))
  expect_equal((records %>% getByName("MAIN"))@statements %>% getByIndex(1), Equation("THALF", "12"))
  expect_equal((records %>% getByName("ODE"))@statements %>% getByIndex(1), Ode("A_CENTRAL", "-K*A_CENTRAL"))    
})

test_that("getCompartments method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  compartments <- model@compartments
  compartment1 <- compartments %>% getByIndex(Compartment(index=1))
  compartment2 <- compartments %>% getByIndex(Compartment(index=2))
  
  expect_equal(compartments %>% length(), 2)
  expect_equal(compartment1@name, "CENTRAL")
  expect_equal(compartment2@name, "OUTPUT")
})

test_that("removeEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  expect_equal(model@model %>% getByName("MAIN") %>% length(), 3) # 3 equations: K, V, S1
  
  model <- model %>% delete(Equation("S1"))
  expect_equal(model@model %>% getByName("MAIN") %>% length(), 2) # 2 equations: K, V
})

test_that("replaceEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  expect_equal(model@model %>% getByName("MAIN") %>% length(), 3) # 3 equations: K, V, S1
  
  model <- model %>% replaceEquation("S1", rhs="V/1000")
  equation <- model %>% find(Equation("S1"))
  expect_equal(equation@rhs, "V/1000") # Equation well modified
})

test_that("addEquation method is working well on code record", {
  
  model <- getNONMEMModelTemplate(1,1)
  
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

test_that("getEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  equation <- model %>% find(Equation("V"))
  expect_equal(equation@rhs, "THETA_V*exp(ETA_V)")
  expect_equal(model %>% getEquation("V2"), NULL)
})

test_that("hasEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  expect_true(model %>% find(Equation("V")) %>% length() > 0)
  expect_false(model %>% find(Equation("V2")) %>% length() > 0)
})

