
library(testthat)

context("Test all methods from the code records class")

testFolder <<- ""
testFolder <<- "C:/prj/pmxmod/tests/testthat/"

test_that("Add and get methods", {
  
  model <- new("code_records",  list=list()) 
  model <- model %>% addRecord(new("pk_record", code=""))
  model <- model %>% addRecord(new("error_record", code=""))
  
  expect_true(model %>% getRecord("PK") %>% length() > 0)
  expect_true(model %>% getRecord("ERROR") %>% length() > 0)
  expect_true(model %>% getRecord("DES") %>% length() == 0)
})

test_that("Write/Read methods", {
  
  model1 <- new("code_records",  list=list()) 
  model1 <- model1 %>% addRecord(new("pk_record", code=c("A=1", "B=2")))
  model1 <- model1 %>% addRecord(new("error_record", code=c("C=3")))
  
  # Write model
  model1 %>% write(file=paste0(testFolder, "write/records/records1.mod"))
  
  # Read model
  model2 <- read.model(file=paste0(testFolder, "write/records/records1.mod"))
  
  expect_equal(model, model)
})
