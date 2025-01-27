
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

test_that("Sampling the OMEGAs and SIGMAs based on the chi-square or inverse wishart distributions work as expected", {
  
  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name="VC_CL", index=2, index2=3, value=0.8, type="cor"))
  
  repModel <- model %>% replicate(10, settings=ReplicationSettings(wishart=TRUE, nsub=100, nobs=1000))
  
  
})
  


