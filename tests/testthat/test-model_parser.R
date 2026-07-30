
library(testthat)

context("Test all methods from model_parser.R")

test_that("parse_if_statement works well (1)", {
  line <- "if(ID > (30))TVCL=THETA_7*pow(0.009*TBW, THETA_8)"
  ifStatement <- parse_statements(line) %>% get_by_index(1)
  
  expect_equal(ifStatement@condition, "ID > (30)")
  expect_equal(ifStatement@equation, Equation("TVCL", "THETA_7*pow(0.009*TBW, THETA_8)"))
})

test_that("parse_if_statement works well (2)", {
  line <- "  if  (ID > (30)  )  TVCL   =   THETA_7*pow(0.009*TBW, THETA_8)  "
  ifStatement <- parse_statements(line) %>% get_by_index(1)
  
  expect_equal(ifStatement@condition, "ID > (30)")
  expect_equal(ifStatement@equation, Equation("TVCL", "THETA_7*pow(0.009*TBW, THETA_8)"))
})

test_that("parse_if_statement works well (3)", {
  line <- "  if  (ID == 30 )  TVCL   =   THETA_7*pow(0.009*TBW, THETA_8)  "
  ifStatement <- parse_statements(line) %>% get_by_index(1)

  expect_equal(ifStatement@condition, "ID == 30")
  expect_equal(ifStatement@equation, Equation("TVCL", "THETA_7*pow(0.009*TBW, THETA_8)"))
})
