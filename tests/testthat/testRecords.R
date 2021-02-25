
library(testthat)

context("Test all methods from the records class")

testFolder <<- ""
testFolder <<- "C:/prj/pmxmod/tests/testthat/"

test_that("Add and get methods", {
  
  records <- new("records",  list=list()) 
  records <- records %>% addRecord(new("pk_record", code=""))
  records <- records %>% addRecord(new("error_record", code=""))
  
  expect_true(records %>% getRecord("PK") %>% length() > 0)
  expect_true(records %>% getRecord("ERROR") %>% length() > 0)
  expect_true(records %>% getRecord("DES") %>% length() == 0)
})

test_that("Write/Read methods", {
  
  records <- new("records",  list=list()) 
  records <- records %>% addRecord(new("pk_record", code=c("A=1", "B=2")))
  records <- records %>% addRecord(new("error_record", code=c("C=3")))
  
  # Write model
  records %>% write(file=paste0(testFolder, "write/records/records1.mod"))
  
  # Read model
  records2 <- read.model(file=paste0(testFolder, "write/records/records1.mod"))
  
  expect_equal(records, records2)
})
