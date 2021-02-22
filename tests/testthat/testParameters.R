
library(testthat)

context("Test all methods from the parameters class")

testFolder <<- ""
testFolder <<- "C:/prj/pmxmod/tests/testthat/"

test_that("Write/Read THETA's", {
  
  file <- paste0(testFolder, "write/", "thetas.csv")
  
  # Read THETA's
  theta1 <- new("theta", name="THETA_CL", index=as.integer(1), suffix="CL", fix=TRUE, value=5)
  theta2 <- new("theta", name="THETA_KA", index=as.integer(2), suffix="KA", fix=TRUE, value=1)
  theta3 <- new("theta", name="THETA_V", index=as.integer(3), suffix="V", fix=TRUE, value=80)
  thetas <- new("parameters", list=c(theta1, theta2, theta3))
  thetas %>% write(file=file)
  
  # Write THETA's
  thetas2 <- read(file, type="theta")
  
  # Check we can retrieve the exact same list
  expect_equal(thetas@list, thetas2@list)
})

