
library(testthat)

context("Test all methods from utilities.R")

test_that("trim method is working well", {
  expect_equal(trim("  hello  "), "hello")
})

test_that("isODE method is working well", {
  expect_true(isODE("d/dt(A_DEPOT)=-KA*A_DEPOT"))
  expect_true(isODE("d/dt(A_OUTPUT)=K*A_CENTRAL"))
  expect_false(isODE("d/dtHELLO(A_OUTPUT)=K*A_CENTRAL"))
  expect_true(isODE("d/dt (A_OUTPUT) = K*A_CENTRAL"))
})

test_that("isLagTime method is working well", {
  expect_true(isLagTime("lag(A_DEPOT)=ALAG1"))
  expect_true(isLagTime("lag (A_DEPOT)=ALAG1"))
  expect_true(isLagTime("lag (A_DEPOT) =ALAG1"))
  expect_true(isLagTime("  lag (A_DEPOT) =  ALAG1"))
  expect_false(isLagTime("  lage (A_DEPOT) =  ALAG1"))
})

test_that("isBioavailibility method is working well", {
  expect_true(isBioavailibility(" f(A_DEPOT)=F1"))
  expect_false(isBioavailibility("lag(A_DEPOT)=ALAG1"))
})

test_that("isInfusionDuration method is working well", {
  expect_true(isInfusionDuration("dur(A_DEPOT)=D1"))
  expect_false(isInfusionDuration("lag(A_DEPOT)=ALAG1"))
})

test_that("isRate method is working well", {
  expect_true(isRate("rate(A_DEPOT)=R1"))
  expect_false(isRate("lag(A_DEPOT)=ALAG1"))
})

test_that("extractTextBetweenBrackets method is working well", {
  expect_equal(extractTextBetweenBrackets("d/dt(A_DEPOT)=-KA*A_DEPOT"), "A_DEPOT")
  expect_error(extractTextBetweenBrackets(c("d/dt(A_DEPOT)=-KA*A_DEPOT", "X=1")))
})

test_that("isEquation is working well", {
  expect_true(isEquation("V3=THETA_V3*VDBW"))
  expect_false(isEquation("if (OCC == 1) VIS1=1"))
  expect_false(isEquation("THETA_V3"))
})

test_that("isInitialCondition is working well", {
  expect_true(isInitialCondition("A_Gluc_X1(0) = X"))
  expect_true(isInitialCondition("A_Gluc_X1( 0 ) = X"))
  expect_true(isInitialCondition("A_Gluc_X1 ( 0 ) = X"))
  expect_false(isInitialCondition("A_Gluc_X1(85) = X"))
})

test_that("getInitialConditionCmt is working well", {
  expect_equal(getInitialConditionCmt("A_Gluc_X1(0) = X"), "A_Gluc_X1")
  expect_equal(getInitialConditionCmt(" A_Gluc_X1 (0 ) = X"), "A_Gluc_X1")
})

test_that("isComment is working well", {
  expect_true(isComment("# HELLO"))
  expect_true(isComment("   # HELLO"))
  expect_true(isComment("\t# HELLO"))
  expect_false(isComment("A# HELLO"))
  expect_false(isComment(" A # HELLO"))
  expect_false(isComment("V3=THETA_V3*VDBW"))
  expect_false(isComment("V3=THETA_V3*VDBW # COMMENT")) # This is an equation that has a comment
})

test_that("hasComment is working well", {
  expect_true(hasComment("# HELLO"))
  expect_true(hasComment("   # HELLO"))
  expect_true(hasComment("\t# HELLO"))
  expect_true(hasComment("V3=THETA_V3*VDBW # COMMENT"))
  expect_false(hasComment("V3=THETA_V3*VDBW"))
})

test_that("isEmptyLine is working well", {
  expect_true(isEmptyLine(""))
  expect_true(isEmptyLine("  "))
  expect_true(isEmptyLine("\t"))
  expect_false(isEmptyLine("\tA"))
})

test_that("extractLhs and extractRhs with comment works well", {
  expect_equal(extractLhs("KA=THETA_KA*exp(ETA_KA) # Comment", split="#"), "KA=THETA_KA*exp(ETA_KA) ")
  expect_equal(extractRhs("KA=THETA_KA*exp(ETA_KA) # Comment", split="#"), " Comment")
})

test_that("isIfStatement works well", {
  line <- "  if (ID > 30) TVCL=THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(isIfStatement(line))
  
  line <- "  if (ID > (30)) TVCL=THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(isIfStatement(line))
  
  line <- " if(ID == 30) TVCL = THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(isIfStatement(line))
})
