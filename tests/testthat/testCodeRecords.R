
library(testthat)

context("Test all methods from the code records class")

testFolder <<- ""

test_that("Add and get methods", {
  
  model <- CodeRecords()
  model <- model %>% add(PkRecord(code=""))
  model <- model %>% add(ErrorRecord(code=""))
  
  expect_true(model %>% getByName("PK") %>% length() > 0)
  expect_true(model %>% getByName("ERROR") %>% length() > 0)
  expect_true(model %>% getByName("DES") %>% length() == 0)
})

test_that("Add not record objects", {
  model <- CodeRecords()
  expect_error(model %>% add(Theta(index=1))) # Element 'THETA_1' does not extend type 'code_record'.
})

test_that("Write/Read methods", {
  
  model1 <- CodeRecords()
  model1 <- model1 %>% add(PkRecord(code=c("A=1", "B=2")))
  model1 <- model1 %>% add(ErrorRecord(code=c("C=3")))
  
  # Write model
  model1 %>% write(file=paste0(testFolder, "write/records/records1.mod"))
  
  # Read model
  model2 <- read.model(file=paste0(testFolder, "write/records/records1.mod"))
  
  expect_equal(model1, model2)
})
