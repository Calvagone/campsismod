library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import '1-cpt fo' Campsis model in JSON format", {
  
  # Import the 1-cpt PK model from JSON
  model <- loadFromJSON(CampsisModel(), file.path(testFolder, "json_examples", "1cpt_fo_model.json"))
  expect_equal(model_suite$pk$`1cpt_fo`, model)
  
  # Export to JSON and re-import
  json <- model %>%
    exportToJSON()
  model <- loadFromJSON(CampsisModel(), json)
  expect_equal(model_suite$pk$`1cpt_fo`, model)
  
  # Import an empty Campsis model from JSON
  model <- loadFromJSON(CampsisModel(), "{\"code\":[], \"parameters\":[]}")
  expect_equal(CampsisModel(), model)
  
  # Import the 1-cpt PK model with a correlation between CL and VC from JSON
  model <- loadFromJSON(CampsisModel(),
                        file.path(testFolder, "json_examples", "1cpt_fo_model_cl_vc_cor.json"))
  expectedModel <- model_suite$pk$`1cpt_fo` %>%
    add(Omega(name="CL_VC", index=3, index2=2, value=0.75, type="cor")) %>%
    campsismod::sort()
  expect_equal(expectedModel, model)
})


test_that("Export/Re-import '2cpt_zo_allo_metab_effect_on_cl' Campsis model to/from JSON", {
  model <- model_suite$testing$other$`2cpt_zo_allo_metab_effect_on_cl`
  
  tmp <- tempfile(fileext=".json") # gsub("\\\\", "/", normalizePath(tmp))
  model %>%
    exportToJSON() %>%
    write(file=tmp)
  
  model2 <- loadFromJSON(CampsisModel(), paste0(readLines(tmp), collapse="\n"))
  expect_equal(model, model2)
})

test_that("Export/Re-import basic test model assembled on the fly to/from JSON", {
  model <- model_suite$pk$`1cpt_fo` %>%
    add(Omega(name="CL_VC", index=3, index2=2, value=0.5, type="cor")) %>% # Add correlation between CL and VC
    standardise() %>%
    addRSE(Theta(name="VC"), value=10) %>% # 10% RSE on VC
    addRSE(Theta(name="CL"), value=10) %>% # 10% RSE on CL
    addRSE(Omega(name="VC"), value=50) %>% # 50% RSE on OMEGA_VC
    addRSE(Omega(name="CL"), value=50) %>% # 50% RSE on OMEGA_CL
    addRSE(Omega(name="CL_VC"), value=100) %>% # 100% RSE on correlation
    addRSE(Sigma(name="PROP_RUV"), value=5) %>% # 5% RSE on proportional error
    campsismod::sort() # Correlation will be put at the right place in the list
  
  tmp <- tempfile(fileext=".json") # gsub("\\\\", "/", normalizePath(tmp))
  json <- model %>%
    exportToJSON()
  json %>%
    write(file=tmp)
  jsonVarcov <- json@data$varcov
  expect_equal(length(jsonVarcov), 6) # We expect exactly 6 values (since zeroes are omitted)
  
  model2 <- loadFromJSON(CampsisModel(), paste0(readLines(tmp), collapse="\n"))
  expect_equal(model, model2)
  
  # Test error catching
  tmp <- tempfile(fileext=".json") # gsub("\\\\", "/", normalizePath(tmp))
  json@data$varcov[[1]]$ref1$name <- "UNKNOWN"
  json %>%
    write(file=tmp)
  expect_error(loadFromJSON(CampsisModel(), paste0(readLines(tmp), collapse="\n")),
               regexp="Parameter reference not found")
})


