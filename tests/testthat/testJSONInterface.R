library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import Campsis model in JSON format", {
  model <- loadFromJSON(CampsisModel(), file.path(testFolder, "json_examples", "example_1cpt_fo.json"))
  expect_equal(model_suite$pk$`1cpt_fo`, model)
})
