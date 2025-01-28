
library(testthat)

context("Test the replication of the Campsis model")

source(paste0("", "testUtils.R"))

test_that("Method 'replicate' allows to replicate a model based on its variance-covariance matrix", {
  
  set.seed(123)
  
  model <- model_suite$pk$`1cpt_fo` %>%
    setMinMax(Theta(name="KA"), min=0.9, max=1.1) %>%
    setMinMax(Theta(name="CL"), min=2.8, max=3.2) %>%
    addRSE(Theta(name="KA"), value=10) %>%
    addRSE(Theta(name="CL"), value=10)
  
  repModel <- model %>% replicate(1000)
  
  model1 <- repModel %>% export(dest=CampsisModel(), index=1)
  expect_equal(model1 %>% find(Theta(name="KA")) %>% .@value, 1.09958, tolerance=1e-4)
  expect_equal(model1 %>% find(Theta(name="CL")) %>% .@value, 2.831857, tolerance=1e-4)
})

test_that("Sampling the OMEGAs and SIGMAs based on the scaled inverse chi-square or wishart distributions work as expected", {
  
  set.seed(123)
  
  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name="VC_CL", index=2, index2=3, value=0.8, type="cor"))
  
  repModel <- model %>%
    replicate(10000, settings=ReplicationSettings(wishart=TRUE, nsub=50, nobs=1000))
  
  # Standardise model first
  model <- model %>%
    standardise()
  nu <- repModel@settings@nsub
  
  # Check the generated values for OMEGA_DUR
  tauSquaredDur <- model %>% find(Omega(name="DUR")) %>% .@value # Correspond to scale argument of rinvchisq
  x <- repModel@replicated_parameters$OMEGA_DUR
  chiSquaredDur <- nu*tauSquaredDur / x # Chi-squared distribution with nu=df=50
  expect_equal(mean(chiSquaredDur), nu, tolerance=0.01) # Mean = nu = 50 
  expect_equal(var(chiSquaredDur), 2*nu, tolerance=0.01) # Var = 2*nu = 100
  
  # Check the generated values for OMEGA_VC
  tauSquaredVc <- model %>% find(Omega(name="VC")) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_VC
  chiSquaredVc <- nu*tauSquaredVc / x
  expect_equal(mean(chiSquaredVc), nu, tolerance=0.02)
  expect_equal(var(chiSquaredVc), 2*nu, tolerance=0.03)
  
  # Check the generated values for OMEGA_CL
  tauSquaredCl <- model %>% find(Omega(name="CL")) %>% .@value
  x <- repModel@replicated_parameters$OMEGA_CL
  chiSquaredCl <- nu*tauSquaredCl / x
  expect_equal(mean(chiSquaredCl), nu, tolerance=0.02)
  expect_equal(var(chiSquaredCl), 2*nu, tolerance=0.03)
})
  


