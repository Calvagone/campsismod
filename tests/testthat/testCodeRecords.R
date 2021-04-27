
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
  
  model <- model %>% removeEquation("S1")
  expect_equal(model@model %>% getByName("MAIN") %>% length(), 2) # 2 equations: K, V
})

test_that("replaceEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  expect_equal(model@model %>% getByName("MAIN") %>% length(), 3) # 3 equations: K, V, S1
  
  model <- model %>% replaceEquation("S1", rhs="V/1000")
  expect_equal((model@model %>% getByName("MAIN"))@code[3], "S1=V/1000") # Equation well modified
})

test_that("add method is working well on code record", {
  
  model <- getNONMEMModelTemplate(1,1)
  
  pk1 <- model@model %>% getByName("MAIN")
  pk1 <- pk1 %>% add("V2=THETA_V2*exp(ETA_V2)", after="V")
  
  pk2 <- model@model %>% getByName("MAIN")
  pk2 <- pk2 %>% add("V2=THETA_V2*exp(ETA_V2)", before="S1")
  
  expect_equal(pk1, pk2)
  
  pk3 <- model@model %>% getByName("MAIN")
  pk3 <- pk3 %>% add("V2=THETA_V2*exp(ETA_V2)", after=2)
  
  pk4 <- model@model %>% getByName("MAIN")
  pk4 <- pk4 %>% add("V2=THETA_V2*exp(ETA_V2)", before=3)
  
  expect_equal(pk1, pk3)
  expect_equal(pk3, pk4)
})

test_that("getEquation method is working well", {
  
  model <- getNONMEMModelTemplate(1,1)
  expect_equal(model %>% getEquation("V"), "THETA_V*exp(ETA_V)")
  expect_equal(model %>% getEquation("V2"), NULL)
})

