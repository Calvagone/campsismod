
library(testthat)

context("Test the replication of the Campsis model")

source(paste0("", "testUtils.R"))

test_that("Method 'replicate' allows to replicate a model based on its variance-covariance matrix", {
  
  set.seed(123)
  model <- model_suite$pk$`1cpt_fo` %>%
    replace(Theta(name="KA", value=1, min=0.9, max=1.1)) %>%
    replace(Theta(name="CL", value=3, min=2.8, max=3.2)) %>%
    addRSE(Theta(name="KA"), value=10) %>%
    addRSE(Theta(name="CL"), value=10)
  
  repModel <- model %>% replicate(1000)
  
  model1 <- repModel %>% export(dest=CampsisModel(), index=1)
  expect_equal(model1 %>% find(Theta(name="KA")) %>% .@value, 1.09958, tolerance=1e-4)
  expect_equal(model1 %>% find(Theta(name="CL")) %>% .@value, 2.831857, tolerance=1e-4)
})