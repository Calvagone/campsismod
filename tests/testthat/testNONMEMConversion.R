library(testthat)

context("Test the very basic conversion to NONMEM")
dest <- "NONMEM"

test_that("All structural elements can be converted to NONMEM (this is still a basic conversion)", {
  equation <- Equation("KA", "THETA(1)*EXP(ETA(1))", comment="This is a comment")
  expect_equal(equation %>% toString(dest=dest), "KA=THETA(1)*EXP(ETA(1)) ; This is a comment")
  
  linebreak <- LineBreak()
  expect_equal(linebreak %>% toString(dest=dest), "")
  
  ifStatement <- IfStatement("COV.EQ.1", equation=Equation("COV_EFFECT", "0.1"), comment="This is a comment")
  expect_equal(ifStatement %>% toString(dest=dest), "IF (COV.EQ.1) COV_EFFECT=0.1 ; This is a comment")
  
  model <- model_suite$testing$nonmem$advan1_trans1
  central <- model %>% find(Ode("A_CENTRAL"))  
  expect_equal(central %>% toString(dest=dest, model=model), "DADT(1)=-K*A_CENTRAL")
  
  comment <- Comment("This is a comment")
  expect_equal(comment %>% toString(dest=dest), "; This is a comment")
  
  text <- UnknownStatement("THIS IS UNKNOWN TEXT")
  expect_equal(text %>% toString(dest=dest), "THIS IS UNKNOWN TEXT")
})
