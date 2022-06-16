
library(testthat)

context("Showing uncertainty on parameters works as expected")

test_that("Showing uncertainty in the console works as expected", {

  # This model has a variance-covariance matrix
  model <- model_suite$other$`2cpt_zo_allo_metab_effect_on_cl`
  
  expect_true(any(grepl(pattern="Variance-covariance matrix available", x=capture.output(show(model)))))
  expect_true(any(grepl(pattern="se\\s+rse%", x=capture.output(show(model@parameters %>% select("theta"))))))
  expect_true(any(grepl(pattern="se\\s+rse%", x=capture.output(show(model@parameters %>% select("omega"))))))
  expect_false(any(grepl(pattern="se\\s+rse%", x=capture.output(show(model@parameters %>% select("sigma"))))))
  
  # This model doesn't have a variance-covariance matrix
  model <- model_suite$pk$`2cpt_zo`
  
  expect_true(any(grepl(pattern="No variance-covariance matrix", x=capture.output(show(model)))))
  expect_false(any(grepl(pattern="se\\s+rse%", x=capture.output(show(model)))))
  expect_false(any(grepl(pattern="se\\s+rse%", x=capture.output(show(model@parameters %>% select("theta"))))))
})

test_that("Method getUncertainty works as expected", {
  
  # This model has a variance-covariance matrix
  model <- model_suite$other$`2cpt_zo_allo_metab_effect_on_cl`
  
  uncertainty <- model %>% getUncertainty() %>% dplyr::mutate_if(.predicate=is.numeric, .funs=~signif(.x, digits=2))
  expected <- tibble::tibble(name=c("THETA_METAB_CL", "THETA_DUR", "THETA_VC", "THETA_VP", "THETA_Q", "THETA_CL", "THETA_PROP_RUV", "OMEGA_DUR", "OMEGA_VC", "OMEGA_CL", "SIGMA_RUV_FIX"),
                             se=c(0.054, 0.022, 0.032, 0.032, 0.02, 0.036, 0.019, 0.0045, 0.0094, 0.0083, NA),
                             'rse%'=c(17, 5.3, 0.72, 0.6, 1.4, 2.2, 0.96, 33, 19, 19, NA))
  expect_equal(uncertainty, expected)
  
  # This model doesn't have a variance-covariance matrix
  model <- model_suite$pk$`2cpt_zo`
  
  uncertainty <- model %>% getUncertainty()
  expected <- tibble::tibble(name=character(0),
                             se=numeric(0),
                             'rse%'=numeric(0))
  expect_equal(uncertainty, expected)
})
