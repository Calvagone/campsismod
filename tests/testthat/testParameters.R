
library(testthat)

context("Test all methods from the parameters class")

testFolder <<- ""

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
  expect_equal(thetas, thetas2)
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


test_that("Order parameters method", {
  
  sigma1 <- new("sigma", name=as.character(NA), index=as.integer(1), index2=as.integer(1), value=1, fix=TRUE)
  omega2 <- new("omega", name=as.character(NA), index=as.integer(2), index2=as.integer(2), value=1, fix=TRUE)
  omega1 <- new("omega", name=as.character(NA), index=as.integer(1), index2=as.integer(1), value=1, fix=TRUE)
  theta2 <- new("theta", name=as.character(NA), index=as.integer(2), value=1, fix=TRUE)
  theta1 <- new("theta", name=as.character(NA), index=as.integer(1), value=1, fix=TRUE)
  
  params <- new("parameters", list=c(sigma1, omega2, omega1, theta2, theta1))
  
  orderedParams <- params %>% order()
  
  expect_equal(orderedParams, new("parameters", list=c(theta1, theta2, omega1, omega2, sigma1)))
  
})


