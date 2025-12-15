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
  
})
