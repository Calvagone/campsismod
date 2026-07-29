
library(testthat)

context("Test all methods from utilities.R")

test_that("trim method is working well", {
  expect_equal(trim("  hello  "), "hello")
})

test_that("is_ode method is working well", {
  expect_true(is_ode("d/dt(A_DEPOT)=-KA*A_DEPOT"))
  expect_true(is_ode("d/dt(A_OUTPUT)=K*A_CENTRAL"))
  expect_false(is_ode("d/dtHELLO(A_OUTPUT)=K*A_CENTRAL"))
  expect_true(is_ode("d/dt (A_OUTPUT) = K*A_CENTRAL"))
})

test_that("extract_text_between_brackets method is working well", {
  expect_equal(extract_text_between_brackets("d/dt(A_DEPOT)=-KA*A_DEPOT"), "A_DEPOT")
  expect_error(extract_text_between_brackets(c("d/dt(A_DEPOT)=-KA*A_DEPOT", "X=1")))
})

test_that("is_equation is working well", {
  expect_true(is_equation("V3=THETA_V3*VDBW"))
  expect_false(is_equation("if (OCC == 1) VIS1=1"))
  expect_false(is_equation("THETA_V3"))
})

test_that("is_comment is working well", {
  expect_true(is_comment("# HELLO"))
  expect_true(is_comment("   # HELLO"))
  expect_true(is_comment("\t# HELLO"))
  expect_false(is_comment("A# HELLO"))
  expect_false(is_comment(" A # HELLO"))
  expect_false(is_comment("V3=THETA_V3*VDBW"))
  expect_false(is_comment("V3=THETA_V3*VDBW # COMMENT")) # This is an equation that has a comment
})

test_that("has_comment is working well", {
  expect_true(has_comment("# HELLO"))
  expect_true(has_comment("   # HELLO"))
  expect_true(has_comment("\t# HELLO"))
  expect_true(has_comment("V3=THETA_V3*VDBW # COMMENT"))
  expect_false(has_comment("V3=THETA_V3*VDBW"))
})

test_that("is_empty_line is working well", {
  expect_true(is_empty_line(""))
  expect_true(is_empty_line("  "))
  expect_true(is_empty_line("\t"))
  expect_false(is_empty_line("\tA"))
})

test_that("extract_lhs and extract_rhs with comment works well", {
  expect_equal(extract_lhs("KA=THETA_KA*exp(ETA_KA) # Comment", split="#"), "KA=THETA_KA*exp(ETA_KA) ")
  expect_equal(extract_rhs("KA=THETA_KA*exp(ETA_KA) # Comment", split="#"), " Comment")
})

test_that("is_if_statement works well", {
  line <- "  if (ID > 30) TVCL=THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(is_if_statement(line))
  
  line <- "  if (ID > (30)) TVCL=THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(is_if_statement(line))
  
  line <- " if(ID == 30) TVCL = THETA_7*pow(0.009*TBW, THETA_8)"
  expect_true(is_if_statement(line))
})

test_that("is_strict_record_delimiter works well", {
  line <- "[MAIN]"
  expect_true(is_strict_record_delimiter(line))
  
  line <- " [MAIN] "
  expect_true(is_strict_record_delimiter(line))
  
  line <- " [  MAIN\t]"
  expect_true(is_strict_record_delimiter(line))
  
  line <- "{MAIN]"
  expect_false(is_strict_record_delimiter(line))
  
  line <- "[MAIN]# COMMENT1 "
  expect_true(is_strict_record_delimiter(line))
  
  line <- "[MAIN] #COMMENT2 "
  expect_true(is_strict_record_delimiter(line))
  
  line <- "[MAIN] A=1"
  expect_false(is_strict_record_delimiter(line))
  expect_true(is_record_delimiter(line))
})

test_that("get_record_delimiter works well", {
  line <- "[MAIN]"
  expect_equal(get_record_delimiter(line), "MAIN")
  
  line <- " [MAIN\t] "
  expect_equal(get_record_delimiter(line), "MAIN")
  
  line <- " [  MAIN  ]"
  expect_equal(get_record_delimiter(line), "MAIN")
  
  line <- "[MAIN] # MAIN block"
  expect_equal(get_record_delimiter(line), "MAIN")
})
