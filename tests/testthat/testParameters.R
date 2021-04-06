
library(testthat)

context("Test all methods from the parameters class")

testFolder <<- ""

test_that("Write/Read THETA's", {
  
  file <- paste0(testFolder, "write/parameters/")
  
  # Read THETA's
  theta1 <- Theta(name="CL", index=1, value=5, fix=TRUE)
  theta2 <- Theta(name="KA", index=2, value=1, fix=TRUE)
  theta3 <- Theta(name="V", index=3,  value=80, fix=TRUE)
  
  thetas <- Parameters() %>% add(theta1) %>% add(theta2) %>% add(theta3)
  thetas %>% write(file=file)
  
  # Write THETA's
  thetas2 <- read.parameters(paste0(file, "theta.csv"), type="theta")
  
  # Check we can retrieve the exact same list
  expect_equal(thetas, thetas2)
})


test_that("Add/contains methods", {
  
  theta1 <- Theta(name="CL", index=1, value=5, fix=TRUE)
  theta2 <- Theta(name="KA", index=2, value=1, fix=TRUE)
  thetas <- Parameters() %>% add(theta1) %>% add(theta2)

  expect_true(thetas %>% contains(Theta(name="CL", index=1)))
  expect_false(thetas %>% contains(Theta(name="V", index=3)))
  
  thetas <- thetas %>% add(Theta(name="V", index=3))
  expect_true(thetas %>% contains(Theta(name="V", index=3)))
})

test_that("GetByIndex, select method", {
  p <- Parameters()
  p <- p %>% add(Theta(index=1))
  p <- p %>% add(Theta(index=2))
  p <- p %>% add(Theta(index=3))
  p <- p %>% add(Omega(index=1, index2=1))
  p <- p %>% add(Omega(index=2, index2=2))
  p <- p %>% add(Sigma(index=1, index2=1))

  expect_equal(p %>% getByIndex(Theta(index=3)), Theta(index=3))
  expect_equal(p %>% getByIndex(Omega(index=1, index2=1)), Omega(index=1, index2=1))
  expect_equal(p %>% getByIndex(Sigma(index=1, index2=1)), Sigma(index=1, index2=1))
  
  p <- p %>% select("theta")
  expect_equal(p %>% length(), 3)
  
  # Check there is no conflict with dplyr::select
  df <- data.frame(ID=c(1,2,3), ID2=c(3,4,5))
  df <- df %>% select(ID)
  expect_false("ID2" %in% colnames(df))
})


test_that("Sort method", {
  
  sigma1 <- Sigma(index=1, index2=1, value=1)
  omega2 <- Omega(index=2, index2=2, value=1)
  omega1 <- Omega(index=1, index2=1, value=1)
  theta2 <- Theta(index=2, value=1)
  theta1 <- Theta(index=1, value=1)
  
  params <- Parameters() %>% add(sigma1) %>% add(omega2) %>% add(omega1) %>% add(theta2) %>% add(theta1)
  
  orderedParams <- params %>% sort()
  
  expectedParams <- Parameters() %>% add(theta1) %>% add(theta2) %>% add(omega1) %>% add(omega2) %>% add(sigma1)
  expect_equal(orderedParams, expectedParams)
  
})

test_that("Disable method (IIV/RUV)", {
  
  model <- getNONMEMModelTemplate(4,4)
  
  model <- model %>% disable("IIV")
  expect_equal((model@parameters %>% getByName("OMEGA_KA"))@value, 0)
  expect_equal((model@parameters %>% getByName("OMEGA_CL"))@value, 0)
  expect_equal((model@parameters %>% getByName("OMEGA_V2"))@value, 0)
  expect_equal((model@parameters %>% getByName("OMEGA_V3"))@value, 0)
  expect_equal((model@parameters %>% getByName("OMEGA_Q"))@value, 0)
  expect_equal((model@parameters %>% getByName("SIGMA_PROP"))@value, 0.025)
  
  model <- model %>% disable("RUV")
  expect_equal((model@parameters %>% getByName("SIGMA_PROP"))@value, 0)
  
  # Error: Erreur : Only these 2 variabilities can be disabled for now: 'IIV', 'RUV'
  expect_error(model %>% disable("IOV"))
})

test_that("Disable method (VARCOV)", {
  
  model <- model_library$my_model1
  expect_equal(model@parameters@varcov %>% length(), 49) # 7*7 matrix
  
  model <- model %>% disable("VARCOV")
  expect_equal(model@parameters@varcov %>% length(), 0)
})
