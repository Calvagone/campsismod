
library(testthat)

context("Test utilities on the Campsis model")

source(paste0("", "testUtils.R"))

test_that("Method 'add' works as expected", {
  
  model <- model_suite$testing$nonmem$advan4_trans4
  
  # Add a parameter
  model <- model %>% add(Theta(name="XX", index=6, value=1))
  expect_equal(model@parameters %>% select("theta") %>% length(), 6)
  
  # Add a code record
  # TODO: update this
  # model <- model %>% add(PredRecord())
  # model@model <- model@model %>% sort()
  # expect_equal(model@model %>% length(), 4)
})

test_that("Method 'replace' works as expected", {
  
  model <- model_suite$testing$nonmem$advan4_trans4
  
  # Replace a parameter (1)
  model <- model %>% replace(Theta(name="KA", index=1, value=1.5))
  expect_equal((model %>% find(Theta("KA")))@value, 1.5)
  
  # Replace a parameter, index does not need to be provided (2)
  model <- model %>% replace(Theta(name="KA", value=1.5))
  expect_equal((model %>% find(Theta("KA")))@value, 1.5)
  
  # Replace a code record
  error <- ErrorRecord() # 0 statement
  model <- model %>% replace(error)
  expect_true(!is.null(model %>% find(ErrorRecord())))
  expect_equal(model %>% find(ErrorRecord()) %>% length(), 0)
  
  # Replace a model statement
  model <- model %>% replace(Equation("S2", "V2*1000"))
  equation <- model %>% find(Equation("S2"))
  expect_equal(equation@rhs, "V2*1000")
  
  # Add and replace a compartment property
  model <- model %>% add(Bioavailability(1, "0.75"))
  model <- model %>% replace(Bioavailability(1, "0.50"))
  expect_equal((model@compartments@properties %>% getByIndex(1))@rhs, "0.50")
})

test_that("Method 'delete' works as expected", {
  
  model <- model_suite$testing$nonmem$advan4_trans4
  
  # Delete a parameter
  updatedModel <- model %>% delete(Theta(name="KA"))
  expect_equal(updatedModel@parameters %>% length(), model@parameters %>% length() - 1)
  
  # Delete a code record
  updatedModel <- model %>% delete(ErrorRecord())
  expect_equal(updatedModel@model %>% length(), model@model %>% length() - 1)
  
  # Delete a model statement
  updatedModel <- model %>% delete(Equation("S2"))
  expect_equal(updatedModel %>% find(MainRecord()) %>% length(), model %>% find(MainRecord()) %>% length() - 1)
  
  # Add and delete a compartment property
  model <- model %>% add(Bioavailability(1, "0.75"))
  model <- model %>% add(Bioavailability(2, "0.75"))
  updatedModel <- model %>% delete(Bioavailability(1))
  expect_equal(updatedModel@compartments@properties %>% length(), model@compartments@properties %>% length() - 1)
  
  # Delete works also with an index
  main <- model %>% find(MainRecord())
  expect_true(main %>% contains(Equation("S2")))
  updatedMain <- main %>% delete(Equation("S2"))
  expect_false(updatedMain %>% contains(Equation("S2")))
})

test_that("Method 'add' on Campsis model, exceptions on parameters names", {
  model1 <- model_suite$testing$nonmem$advan4_trans4
  model2 <- model_suite$testing$nonmem$advan1_trans1
  expect_error(model1 %>% add(model2), regexp="Model can't be appended because of duplicate parameter name\\(s\\): EPS_PROP")
  
  model1 <- model_suite$testing$nonmem$advan1_trans1
  model2 <- model_suite$testing$nonmem$advan1_trans1
  expect_error(model1 %>% add(model2), regexp="Model can't be appended because of duplicate parameter name\\(s\\): THETA_K, THETA_V, ETA_K, ETA_V, EPS_PROP")
  
  # Unnamed correlations should not be an issue when merging models
  model1 <- model_suite$testing$nonmem$advan1_trans1 %>%
    addSuffix(suffix="1") %>%
    add(Omega(index=2, index2=1, value=0.5, type="cor"))
  model2 <- model_suite$testing$nonmem$advan1_trans1 %>%
    addSuffix(suffix="2") %>%
    add(Omega(index=2, index2=1, value=0.5, type="cor"))
  resultingModel <- model1 %>% add(model2)
  
  # Check correlations are still there at the correct omega indexes
  expect_equal(resultingModel@parameters %>% getByIndex(Omega(index=2, index2=1)) %>% .@type, "cor")
  expect_equal(resultingModel@parameters %>% getByIndex(Omega(index=4, index2=3)) %>% .@type, "cor") 
})

test_that("Method 'add' on Campsis model, exceptions on compartment names", {
  model1 <- model_suite$testing$nonmem$advan4_trans4
  model1@parameters@list <- model1@parameters@list[-(model1@parameters %>% length())] # Remove SIGMA PROP
  
  model2 <- model_suite$testing$nonmem$advan1_trans1
  model2@parameters@list <- model2@parameters@list[-(model2@parameters %>% length())] # Remove SIGMA PROP
  
  expect_error(model1 %>% add(model2), regexp="Element 'ODE \\(A_CENTRAL\\)' is already present.")
  #expect_error(model1 %>% add(model2), regexp="Model can't be appended because of duplicate compartment name\\(s\\): A_CENTRAL, A_OUTPUT")
})

test_that("Add effect compartment model to PK model using add method", {
  regFilename <- "pk_pd_appended"
  
  pk <- model_suite$testing$nonmem$advan4_trans4
  pk <- pk %>% add(Bioavailability(1, "0.75"))
  
  pd <- new("campsis_model") %>%
    add(Equation("KE0", "THETA_KE0*exp(ETA_KE0)")) %>%
    add(Ode("A_EFFECT", "KE0*(A_CENTRAL/S2 - A_EFFECT)"))
  
  expect_equal(pd@compartments %>% length(), 1)
  
  pd <- pd %>% add(Theta("KE0", value=0.5))
  pd <- pd %>% add(Omega("KE0", value=0.3, type="sd"))
  
  pd <- pd %>% add(InitialCondition(1, "0.40"))
  pd <- pd %>% add(Bioavailability(1, "0.50"))
  
  pkpd <- pk %>% add(pd)
  
  campsisNonRegTest(pkpd, regFilename)
})

test_that("Valid object method works depending on complete argument", {
  model <- model_suite$testing$nonmem$advan4_trans4
  model@parameters@list[[1]]@index <- c(1,1) %>% as.integer()
  
  expect_true(validObject(model))
  expect_error(validObject(model, complete=TRUE))
})

test_that("Method 'add' properly merges variance-covariance matrices", {
  modelA <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    addSuffix("A") %>%
    disable("VARCOV_OMEGA")
  
  modelB <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    addSuffix("B")
  
  model <- modelA %>%
    add(modelB)
  
  varcov <- model %>%
    getVarCov()
  
  varcovExpected <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl` %>%
    getVarCov()
  
  # Check variance-covariance content
  expect_equal(as.numeric(varcov[1:7, 1:7]), as.numeric(varcovExpected[1:7, 1:7]))
  expect_equal(as.numeric(varcov[8:17, 8:17]), as.numeric(varcovExpected))
  dimnames <- dimnames(varcov)
  expect_equal(dimnames[[1]], dimnames[[2]])
  
  expected <- c("THETA_METAB_CL_A", "THETA_DUR_A", "THETA_VC_A", "THETA_VP_A", "THETA_Q_A", "THETA_CL_A", "THETA_PROP_RUV_A",
                "THETA_METAB_CL_B", "THETA_DUR_B", "THETA_VC_B", "THETA_VP_B", "THETA_Q_B", "THETA_CL_B", "THETA_PROP_RUV_B",
                "OMEGA_DUR_B", "OMEGA_VC_B", "OMEGA_CL_B")
  expect_equal(expected, dimnames[[1]])
  
  # Add the empty model to the model with variance-covariance matrix
  expect_equal(as.numeric(modelB %>% add(CampsisModel()) %>% getVarCov()), as.numeric(varcovExpected))
  
  # Add the model with variance-covariance matrix to the empty model
  expect_equal(as.numeric(CampsisModel() %>% add(modelB) %>% getVarCov()), as.numeric(varcovExpected))
})

test_that("Method 'addRSE' works as expected", {
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    addRSE(Theta("CL"), 10) %>%
    addRSE(Theta("Q"), 8) %>% # Test #92 (addRSE can be called multiple times)
    addRSE(Theta("Q"), 10) %>%
    addRSE(Omega("KA"), 50)
  
  uncertainty <- getUncertainty(model) %>%
    dplyr::filter(!is.na(.data$`rse%`))
  
  expect_equal(tibble::tibble(name=c("THETA_CL", "THETA_Q", "OMEGA_KA"), se=c(0.5, 0.4, 0.0125), `rse%`=c(10, 10, 50)), uncertainty)
})

test_that("Method 'move' works as expected", {
  # Move CP from error block to ODE block
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    move(Equation("CP"), Position(OdeRecord()))
  
  # Check that CP is now in the ODE block
  expect_true(model %>% find(OdeRecord()) %>% contains(Equation("CP")))
  
  # Check that CP is not in the error block
  expect_false(model %>% find(ErrorRecord()) %>% contains(Equation("CP")))
  
  # Move the error block to the ODE block
  model <- model_suite$testing$nonmem$advan4_trans4 %>%
    move(ErrorRecord(), Position(OdeRecord()))
  
  # Check error block is empty
  expect_equal(model %>% find(ErrorRecord()) %>% length(), 0)
  expect_true(model %>% find(OdeRecord()) %>% contains(Equation("CP")))
  expect_true(model %>% find(OdeRecord()) %>% contains(Equation("OBS_CP")))
  expect_true(model %>% find(OdeRecord()) %>% contains(Equation("Y")))
})



