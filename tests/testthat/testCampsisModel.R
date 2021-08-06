
library(testthat)

context("Test CAMPSIS model")

testFolder <<- ""
overwriteNonRegressionFiles <<- FALSE

source(paste0(testFolder, "testUtils.R"))

test_that("getCompartmentIndex method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  index_depot <- model %>% getCompartmentIndex("DEPOT")
  index_central <- model %>% getCompartmentIndex("CENTRAL")
  
  expect_equal(index_depot, 1)
  expect_equal(index_central, 2)
  expect_error(model %>% getCompartmentIndex("XX"))
})

test_that("add method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  
  # Add a parameter
  model <- model %>% add(Theta(name="XX", index=6, value=1))
  expect_equal(model@parameters %>% select("theta") %>% length(), 6)
  
  # Add a code record
  # TODO: update this
  # model <- model %>% add(PredRecord())
  # model@model <- model@model %>% sort()
  # expect_equal(model@model %>% length(), 4)
})

test_that("replace method works well", {
  
  model <- getNONMEMModelTemplate(4,4)
  
  # Replace a parameter
  model <- model %>% replace(Theta(name="KA", index=1, value=1.5))
  expect_equal((model@parameters %>% getByName("THETA_KA"))@value, 1.5)
  
  # Replace a code record
  error <- ErrorRecord()
  model <- model %>% replace(error)
  expect_equal(model@model %>% getByName("ERROR") %>% length(), 0)
})

test_that("add method on PMX model, exceptions on parameters names", {
  model1 <- getNONMEMModelTemplate(4,4)
  model2 <- getNONMEMModelTemplate(1,1)
  expect_error(model1 %>% add(model2), regexp="Model can't be appended because of duplicate parameter name\\(s\\): SIGMA_PROP")
})

test_that("add method on PMX model, exceptions on compartment names", {
  model1 <- getNONMEMModelTemplate(4,4)
  model1@parameters@list <- model1@parameters@list[-(model1@parameters %>% length())] # Remove SIGMA PROP
  
  model2 <- getNONMEMModelTemplate(1,1)
  model2@parameters@list <- model2@parameters@list[-(model2@parameters %>% length())] # Remove SIGMA PROP
  
  expect_error(model1 %>% add(model2), regexp="Element 'ODE \\(A_CENTRAL\\)' is already present.")
  #expect_error(model1 %>% add(model2), regexp="Model can't be appended because of duplicate compartment name\\(s\\): A_CENTRAL, A_OUTPUT")
})

test_that("add effect compartment model to PK model using add method", {
  regFilename <- "pk_pd_appended"
  
  pk <- getNONMEMModelTemplate(4,4)
  pk <- pk %>% add(Bioavailability(1, "0.75"))
  
  pd <- new("campsis_model")
  
  pd_main <- MainRecord() %>% 
    addEquation("KE0", "THETA_KE0*exp(ETA_KE0)")
  
  pd_ode <- OdeRecord() %>% 
    addEquation("d/dt(A_EFFECT)", "KE0*(A_CENTRAL/S2 - A_EFFECT)")
  
  pd <- pd %>% add(pd_main) %>% add(pd_ode)
  pd <- pd %>% add(Theta("KE0", value=0.5))
  pd <- pd %>% add(Omega("KE0", value=0.3, type="sd"))
  pd <- pd %>% updateCompartments()
  pd <- pd %>% add(InitialCondition(1, "0.40"))
  pd <- pd %>% add(Bioavailability(1, "0.50"))
  
  pkpd <- pk %>% add(pd)
  
  campsisNonRegTest(pkpd, regFilename)
  # dataset <- Dataset(1) %>% add(Bolus(0, 100)) %>% add(Observations(0:48))
  # results <- pkpd %>% simulate(dataset=dataset, dest="RxODE", seed=1)
  # spaghettiPlot(results, "A_EFFECT")
})
