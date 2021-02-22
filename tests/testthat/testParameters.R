
library(testthat)

context("Test all methods from the parameters class")

testFolder <<- ""
#testFolder <<- "C:/prj/pmxmod/tests/testthat/"

test_that("Write/Read THETA's", {
  
  file <- paste0(testFolder, "write/", "thetas.csv")
  
  # Read THETA's
  theta1 <- new("theta", name="CL", index=as.integer(1), value=5, fix=TRUE)
  theta2 <- new("theta", name="KA", index=as.integer(2), value=1, fix=TRUE)
  theta3 <- new("theta", name="V", index=as.integer(3),  value=80, fix=TRUE)
  thetas <- new("parameters", list=c(theta1, theta2, theta3))
  thetas %>% write(file=file)
  
  # Write THETA's
  thetas2 <- read(file, type="theta")
  
  # Check we can retrieve the exact same list
  expect_equal(thetas@list, thetas2@list)
})

test_that("Has/Add parameter method", {
  
  theta1 <- new("theta", name="CL", index=as.integer(1), value=5, fix=TRUE)
  theta2 <- new("theta", name="KA", index=as.integer(2), value=1, fix=TRUE)
  thetas <- new("parameters", list=c(theta1, theta2))

  expect_true(thetas %>% hasParameter(new("theta", name="CL", index=as.integer(1), value=5, fix=TRUE)) %>% length() > 0)
  expect_false(thetas %>% hasParameter(new("theta", name="V", index=as.integer(3), value=80, fix=TRUE)) %>% length() > 0)
  
  thetas <- thetas %>% addParameter(new("theta", name="V", index=as.integer(3), value=80, fix=TRUE))
  expect_true(thetas %>% hasParameter(new("theta", name="V", index=as.integer(3), value=80, fix=TRUE)) %>% length() > 0)
})


