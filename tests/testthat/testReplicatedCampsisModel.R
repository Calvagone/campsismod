
library(testthat)

context("Test the replication of the Campsis model")

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
  
  # The following test was moved from Campsis v1.6.0 to Campsismod v1.2.0
  
  set.seed(1)
  model <- model_suite$testing$other$my_model1
  repModel <- model %>% replicate(100)
  
  thetas <- repModel@replicated_parameters$THETA_CL
  var <- model@parameters@varcov["THETA_CL", "THETA_CL"]
  expect_equal(sd(thetas), sqrt(var), tolerance=1e-2)
  
  omegas <- repModel@replicated_parameters$OMEGA_CL
  var <- model@parameters@varcov["OMEGA_CL", "OMEGA_CL"]
  expect_equal(sd(omegas), sqrt(var), tolerance=1e-3)
})

test_that("Sampling the OMEGAs and SIGMAs based on the scaled inverse chi-square or wishart distributions works as expected", {
  
  set.seed(123)
  
  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    add(Omega(name="VC_CL", index=2, index2=3, value=0.8, type="cor")) %>%
    replace(Sigma(name="RUV_FIX", value=1, type="var", fix=FALSE)) # Unfix the RUV_FIX just for the test
  
  settings <- AutoReplicationSettings(wishart=TRUE, nsub=50, nobs=1000)
  repModel <- model %>%
    replicate(10000, settings=settings)
  
  # Standardise model first
  model <- model %>%
    standardise()
  nu <- settings@nsub
  nuSigma <- settings@nobs
  
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
  
  # Check the generated values for RUV_FIX
  tauSquaredRuv <- model %>% find(Sigma(name="RUV_FIX")) %>% .@value
  x <- repModel@replicated_parameters$SIGMA_RUV_FIX
  chiSquaredRuv <- nuSigma*tauSquaredRuv / x
  expect_equal(mean(chiSquaredRuv), nuSigma, tolerance=0.02)
  expect_equal(var(chiSquaredRuv), 2*nuSigma, tolerance=0.03)
  
  model1 <- repModel %>% export(dest=CampsisModel(), index=1)
  expect_equal(model1 %>% find(Omega(name="DUR")) %>% .@value, 0.01275051, tolerance=1e-4)
  expect_equal(model1 %>% find(Omega(name="VC")) %>% .@value, 0.03926352, tolerance=1e-4)
  expect_equal(model1 %>% find(Omega(name="CL")) %>% .@value, 0.04142518, tolerance=1e-4)
  expect_equal(model1 %>% find(Omega(name="VC_CL")) %>% .@value, 0.03121631, tolerance=1e-4)
  
  model2 <- repModel %>% export(dest=CampsisModel(), index=2)
  expect_equal(model2 %>% find(Omega(name="DUR")) %>% .@value, 0.01582257, tolerance=1e-4)
  expect_equal(model2 %>% find(Omega(name="VC")) %>% .@value, 0.05844814, tolerance=1e-4)
  expect_equal(model2 %>% find(Omega(name="CL")) %>% .@value, 0.05186493, tolerance=1e-4)
  expect_equal(model2 %>% find(Omega(name="VC_CL")) %>% .@value, 0.04422637, tolerance=1e-4)
})

test_that("Method 'setMinMax' can be used on THETAs, OMEGAs and SIGMAs to set limits", {
  
  set.seed(123)
  
  model1 <- model_suite$pk$`1cpt_fo` %>%
    setMinMax("theta", min=0, max=Inf) %>%
    addRSE(Theta(name="KA"), value=100) # Very large RSE on KA
  
  repModel1 <- model1 %>% replicate(1000)
  
  # No THETA_KA should be negative
  expect_true(all(repModel1@replicated_parameters$THETA_KA >= 0))
  
  # Same model but no limit
  model2 <- model_suite$pk$`1cpt_fo` %>%
    addRSE(Theta(name="KA"), value=100) # Very large RSE on KA
  
  repModel2 <- model2 %>% replicate(1000)
  
  # 841 values of THETA_KA are positive
  # 159 values of THETA_KA are negative
  expect_equal(sum(repModel2@replicated_parameters$THETA_KA >= 0), 841)
  expect_equal(sum(repModel2@replicated_parameters$THETA_KA < 0), 159)
})

test_that("Replicate a model that has IOV works as expected", {
  # The following test was moved from Campsis v1.6.0 to Campsismod v1.2.0
  set.seed(1)
  model <- model_suite$testing$nonmem$advan2_trans2
  
  # Add uncertainty on OMEGA_IOV_CL1
  varcov <- matrix(1e-4) # SD=0.01
  row.names(varcov) <- "OMEGA_IOV_CL1"
  colnames(varcov) <- "OMEGA_IOV_CL1"
  model@parameters@varcov <- varcov
  
  pk <- model %>%
    add(Omega(name="IOV_CL1", value=0.025, type="var", same=FALSE)) %>%
    add(Omega(name="IOV_CL2", value=0.025, type="var", same=TRUE)) %>%
    add(Omega(name="IOV_CL3", value=0.025, type="var", same=TRUE))
  
  repModel <- pk %>% replicate(2)
  pk1 <- repModel %>% export(dest=CampsisModel(), index=1)
  pk2 <- repModel %>% export(dest=CampsisModel(), index=2)
  
  set <- c(pk %>% find(Omega("IOV_CL1")) %>% .@value,
           pk %>% find(Omega("IOV_CL2")) %>% .@value,
           pk %>% find(Omega("IOV_CL3")) %>% .@value)
  
  set1 <- c(pk1 %>% find(Omega("IOV_CL1")) %>% .@value,
            pk1 %>% find(Omega("IOV_CL2")) %>% .@value,
            pk1 %>% find(Omega("IOV_CL3")) %>% .@value)
  
  set2 <- c(pk2 %>% find(Omega("IOV_CL1")) %>% .@value,
            pk2 %>% find(Omega("IOV_CL2")) %>% .@value,
            pk2 %>% find(Omega("IOV_CL3")) %>% .@value)
  
  expect_equal(set, c(0.025, 0.025, 0.025))
  expect_equal(round(set1, digits=3), c(0.019, 0.019, 0.019)) # Depends on seed
  expect_equal(round(set2, digits=3), c(0.027, 0.027, 0.027)) # Depends on seed
})
  
test_that("Method 'replicate' also allows to manually replicate a model based on a table (1 replicate/row)", {
  model <- model_suite$testing$nonmem$advan2_trans2
  data <- data.frame(REPLICATE=seq_len(5), SIGMA_PROP=c(0.1, 0.2, 0.3, 0.4, 0.5))
  
  expect_error(model %>% replicate(n=6, settings=ManualReplicationSettings(data)))

  repModel <- model %>% replicate(n=3, settings=ManualReplicationSettings(data))
  expect_equal(repModel@replicated_parameters$SIGMA_PROP, c(0.1, 0.2, 0.3))
  
  sigma1 <- repModel %>%
    export(dest=CampsisModel(), index=1) %>%
    find(Sigma("PROP"))
  expect_equal(sigma1@value, 0.1)
  
  sigma2 <- repModel %>%
    export(dest=CampsisModel(), index=2) %>%
    find(Sigma("PROP"))
  expect_equal(sigma2@value, 0.2)
  # Etc.
})


