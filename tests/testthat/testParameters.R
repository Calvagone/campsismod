
library(testthat)

context("Test all methods from the parameters class")

source(paste0("", "testUtils.R"))

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

  expect_true(thetas %>% contains(Theta(name="CL")))
  expect_false(thetas %>% contains(Theta(name="V")))
  
  thetas <- thetas %>% add(Theta(name="V"))
  expect_true(thetas %>% contains(Theta(name="V")))
})

test_that("GetByIndex, find & select method work well", {
  p <- Parameters()
  p <- p %>% add(Theta())
  p <- p %>% add(Theta())
  p <- p %>% add(Theta())
  p <- p %>% add(Omega())
  p <- p %>% add(Omega())
  p <- p %>% add(Sigma())

  # Search by index
  expect_equal(p %>% getByIndex(Theta(index=3)), Theta(index=3))
  expect_equal(p %>% getByIndex(Omega(index=1, index2=1)), Omega(index=1, index2=1))
  expect_equal(p %>% getByIndex(Sigma(index=1, index2=1)), Sigma(index=1, index2=1))
  
  # Or equivalently using find
  expect_equal(p %>% find(Theta(index=3)), Theta(index=3))
  expect_equal(p %>% find(Omega(index=1, index2=1)), Omega(index=1, index2=1))
  expect_equal(p %>% find(Sigma(index=1, index2=1)), Sigma(index=1, index2=1))
  
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
  
  params <- Parameters() %>% add(c(sigma1, omega2, omega1, theta2, theta1))
  
  orderedParams <- params %>% sort()
  
  expectedParams <- Parameters() %>% add(c(theta1, theta2, omega1, omega2, sigma1))
  expect_equal(orderedParams, expectedParams)
})

test_that("Disable method (IIV/RUV)", {
  
  model <- model_suite$testing$nonmem$advan4_trans4
  
  model <- model %>% disable("IIV")
  expect_equal((model@parameters %>% find(Omega(name="KA")))@value, 0)
  expect_equal((model@parameters %>% find(Omega(name="CL")))@value, 0)
  expect_equal((model@parameters %>% find(Omega(name="V2")))@value, 0)
  expect_equal((model@parameters %>% find(Omega(name="V3")))@value, 0)
  expect_equal((model@parameters %>% find(Omega(name="Q")))@value, 0)
  expect_equal((model@parameters %>% find(Sigma(name="PROP")))@value, 0.025)
  
  model <- model %>% disable("RUV")
  expect_equal((model@parameters %>% find(Sigma(name="PROP")))@value, 0)
  
  expect_error(model %>% disable("SOMETHING"), regexp="Only these variabilities can be disabled")
})

test_that("Disable method (VARCOV)", {
  
  model <- model_suite$testing$other$my_model1
  expect_equal(model@parameters@varcov %>% length(), 49) # 7*7 matrix
  
  model <- model %>% disable("VARCOV")
  expect_equal(model@parameters@varcov %>% length(), 0)
})

test_that("Disable method (VARCOV_OMEGA & VARCOV_SIGMA)", {
  
  model <- model_suite$testing$other$my_model1
  expect_equal(model@parameters@varcov %>% length(), 49) # 7*7 matrix
  
  model <- model %>% disable("VARCOV_OMEGA")
  expect_equal(model@parameters@varcov %>% length(), 25) # 2 OMEGA's removed
  
  model <- model %>% disable("VARCOV_SIGMA")
  expect_equal(model@parameters@varcov %>% length(), 16) # 1 SIGMA removed
  
  # All at once
  model <- model_suite$testing$other$my_model1
  model <- model %>% disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  expect_equal(model@parameters@varcov %>% length(), 16)
})

test_that("Disable method (IOV)", {
  model <- read.campsis(paste0(testFolder, "custom/", "model1_omega_fixed"))
  model <- model %>% disable("IOV")
  
  omega4 <- model@parameters %>% getByIndex(Omega(index=4, index2=4))
  expect_true(omega4@value != 0)
  
  omega5 <- model@parameters %>% getByIndex(Omega(index=5, index2=5))
  expect_true(omega5@value == 0)
  
  omega6 <- model@parameters %>% getByIndex(Omega(index=6, index2=6))
  expect_true(omega6@value == 0)
})

test_that("Fix omega method is working", {
  
  # 'Unfix' OMEGA matrix
  original_model <- read.campsis(paste0(testFolder, "custom/", "model1_omega_fixed"))
  list <- original_model@parameters@list %>% purrr::map(.f=function(x) {
    if (is(x, "omega")) {
      if(isTRUE(x@same)) {
        x@value <- as.numeric(NA)
      }
      x@same <- as.logical(NA)
    }
    return(x)
  })
  model_not_fixed <- original_model
  model_not_fixed@parameters@list <- list
  
  # Fix OMEGA
  model_fixed <- model_not_fixed
  model_fixed@parameters <- model_fixed@parameters %>% fixOmega()
  
  expect_equal(original_model, model_fixed)
})

test_that("Name column is optional", {
  
  model <- read.campsis(paste0(testFolder, "custom/", "advan1_trans1_no_name"))
  names <- (model@parameters %>% select("theta"))@list %>% purrr::map_chr(.f=~.x@name)
  expect_true(all(is.na(names)))
})

test_that("THETA indexes validation", {
  # 2 THETAS OK
  theta1 <- Theta(index=1, value=0)
  theta2 <- Theta(index=2, value=0)
  thetas <- Parameters() %>% add(theta1) %>% add(theta2)
  expect_equal(thetas %>% length(), 2)
  expect_true(validObject(thetas))
  
  # Wrong starting index
  thetas <- Parameters() %>% add(Theta(index=2, value=0))
  expect_error(validObject(thetas))
  
  # NA value
  thetas <- Parameters() %>% add(Theta(index=1, value=NA))
  expect_error(validObject(thetas))
  
  # Index 2 missing
  theta1 <- Theta(index=1, value=0)
  theta2 <- Theta(index=3, value=0)
  thetas <- Parameters() %>% add(theta1) %>% add(theta2)
  expect_equal(thetas %>% length(), 2)
  expect_error(validObject(thetas))
})

test_that("OMEGA indexes validation", {
  # 2 OMEGA OK
  omega1 <- Omega(index=1, index2=1, value=0)
  omega2 <- Omega(index=2, index2=2, value=0)
  omegas <- Parameters() %>% add(omega1) %>% add(omega2)
  expect_equal(omegas %>% length(), 2)
  expect_true(validObject(omegas))
  
  # Wrong starting index
  omegas <- Parameters() %>% add(Omega(index=2, index2=2, value=0))
  expect_error(validObject(omegas))
  
  # NA value
  omegas <- Parameters() %>% add(Omega(index=1, index2=1, value=NA))
  expect_error(validObject(omegas))
  
  # Index 2 missing
  omega1 <- Omega(index=1, index2=1, value=0)
  omega2 <- Omega(index=3, index2=3, value=0)
  omegas <- Parameters() %>% add(omega1) %>% add(omega2)
  expect_equal(omegas %>% length(), 2)
  expect_error(validObject(omegas))
})

test_that("SIGMA indexes validation", {
  # 2 SIGMA OK
  sigma1 <- Sigma(index=1, index2=1, value=0)
  sigma2 <- Sigma(index=2, index2=2, value=0)
  sigmas <- Parameters() %>% add(sigma1) %>% add(sigma2)
  expect_equal(sigmas %>% length(), 2)
  expect_true(validObject(sigmas))
  
  # Wrong starting index
  sigmas <- Parameters() %>% add(Sigma(index=2, index2=2, value=0))
  expect_error(validObject(sigmas))
  
  # NA value
  sigmas <- Parameters() %>% add(Sigma(index=1, index2=1, value=NA))
  expect_error(validObject(sigmas))
  
  # Index 2 missing
  sigma1 <- Sigma(index=1, index2=1, value=0)
  sigma2 <- Sigma(index=3, index2=3, value=0)
  sigmas <- Parameters() %>% add(sigma1) %>% add(sigma2)
  expect_equal(sigmas %>% length(), 2)
  expect_error(validObject(sigmas))
})

test_that("Add parameters with NA indexes", {
  # Thetas
  parameters <- Parameters()
  parameters <- parameters %>% add(Theta(value=0))
  parameters <- parameters %>% add(Theta(value=0))
  parameters <- parameters %>% add(Theta(value=0))
  
  expect_equal(parameters %>% getByIndex(Theta(index=1)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Theta(index=2)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Theta(index=3)) %>% length(), 1)
  expect_true(validObject(parameters))
  
  # Omegas
  parameters <- Parameters()
  parameters <- parameters %>% add(Omega(value=0))
  parameters <- parameters %>% add(Omega(value=0))
  parameters <- parameters %>% add(Omega(value=0))
  
  expect_equal(parameters %>% getByIndex(Omega(index=1, index2=1)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Omega(index=2, index2=2)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Omega(index=3, index2=3)) %>% length(), 1)
  expect_true(validObject(parameters))
  
  # Sigmas
  parameters <- Parameters()
  parameters <- parameters %>% add(Sigma(value=0))
  parameters <- parameters %>% add(Sigma(value=0))
  parameters <- parameters %>% add(Sigma(value=0))
  
  expect_equal(parameters %>% getByIndex(Sigma(index=1, index2=1)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Sigma(index=2, index2=2)) %>% length(), 1)
  expect_equal(parameters %>% getByIndex(Sigma(index=3, index2=3)) %>% length(), 1)
  expect_true(validObject(parameters))
})

test_that("Replace parameters without specifying the index", {
  # Thetas
  parameters <- Parameters()
  parameters <- parameters %>% add(Theta(name="X1", value=0))
  parameters <- parameters %>% add(Theta(name="X2", value=0))
  parameters <- parameters %>% add(Theta(name="X3", value=0))
  parameters <- parameters %>% replace(Theta(name="X1", value=10))
  
  expect_equal((parameters %>% find(Theta(name="X1")))@value, 10)
  
  # Omegas
  parameters <- Parameters()
  parameters <- parameters %>% add(Omega(name="X1", value=0))
  parameters <- parameters %>% add(Omega(name="X2", value=0))
  parameters <- parameters %>% add(Omega(name="X3", value=0))
  parameters <- parameters %>% replace(Omega(name="X1", value=10))
  
  expect_equal((parameters %>% find(Omega(name="X1")))@value, 10)
  
  # Sigmas
  parameters <- Parameters()
  parameters <- parameters %>% add(Sigma(name="X1", value=0))
  parameters <- parameters %>% add(Sigma(name="X2", value=0))
  parameters <- parameters %>% add(Sigma(name="X3", value=0))
  parameters <- parameters %>% replace(Sigma(name="X1", value=10))
  
  expect_equal((parameters %>% find(Sigma(name="X1")))@value, 10)
})

test_that("Add all method works well", {
  # Use c() operator and add
  parameters1 <- Parameters()
  parameters1 <- parameters1 %>% add(c(Theta(value=0), Theta(value=0), Theta(value=0)))
  
  expect_equal(parameters1 %>% length(), 3)
  
  # Use list() operator and add
  parameters2 <- Parameters()
  parameters2 <- parameters2 %>% add(list(Theta(value=0), Theta(value=0), Theta(value=0)))
  
  expect_equal(parameters2 %>% length(), 3)
  expect_equal(parameters1, parameters2)
})

test_that("Max index on empty list works and returns an integer", {
  parameters <- Parameters()
  index <- parameters %>% maxIndex()
  expect_true(is.integer(index))
  expect_equal(index, 0)
})

test_that("Parameters encoded with locale 'French' (semi-colon used as delimiter in CSV file) can be read properly", {
  
  # Model with parameters that were saved into CSV files on laptop with locale French
  # This model correspond to advan1_trans1
  model <- read.campsis(paste0(testFolder, "custom/", "csv_locale_french"))

  # Let's compare it with original model from model library
  expect_equal(model, model_suite$testing$nonmem$advan1_trans1)
})

test_that("Standardise method works as expected", {
  
  params1 <- Parameters() %>%
    add(Omega(name="CL", value=0.2, type="sd")) %>%
    add(Omega(name="V", value=0.3^2, type="var")) %>%
    add(Omega(name="CL_V", index=1, index2=2, value=0.25, type="cor"))
  
  params2 <- Parameters() %>%
    add(Omega(name="CL", value=0.2, type="sd")) %>%
    add(Omega(name="V", value=0.3^2, type="var")) %>%
    add(Omega(name="CL_V", index=1, index2=2, value=0.015, type="covar"))
  
  params_expected <- Parameters() %>%
    add(Omega(name="CL", value=0.2^2, type="var")) %>%
    add(Omega(name="V", value=0.3^2, type="var")) %>%
    add(Omega(name="CL_V", index=1, index2=2, value=0.015, type="covar"))
  
  expect_equal(params1 %>% standardise(), params_expected)
  expect_equal(params2 %>% standardise(), params_expected)
})
