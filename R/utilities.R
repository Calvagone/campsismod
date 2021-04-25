
#' Process extra arguments.
#' 
#' @param args arguments list
#' @param name argument name to retrieve
#' @param default default value if argument is not present
#' @param mandatory mandatory argument, logical value
#' @return requested argument value
#' @export
processExtraArg <- function(args, name, default=NULL, mandatory=FALSE) {
  if (hasName(args, name)) {
    retValue <- args[[name]]
  } else {
    if (is.null(default) && mandatory) {
      stop(paste0("Extra argument '", name, "' is mandatory."))
    }
    retValue <- default
  }
  return(retValue)
}

#' Say if line(s) in record is/are ODE or not.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isODE <- function(x) {
  return(grepl(pattern="^d/dt.*\\(.*\\).*=", x=trim(x), ignore.case=TRUE))
}

#' Say if line in record is an equation not.
#' 
#' @param x character value
#' @return logical value
#' @export
isEquation <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a character value to avoid ambiguities")
  parts <- strsplit(x, split="=")[[1]]
  variable <- parts[1] %>% trim()
  return(grepl(pattern="^[a-zA-Z_][a-zA-Z0-9_]*$", x=variable))
}

#' Say if line(s) in record is/are lag times.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isLagTime <- function(x) {
  return(grepl(pattern="^lag\\s*\\(.*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Say if line(s) in record is/are bioavailabilities.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isBioavailibility <- function(x) {
  return(grepl(pattern="^f\\s*\\(.*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Say if line(s) in record is/are infusion durations.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isInfusionDuration <- function(x) {
  return(grepl(pattern="^dur\\s*\\(.*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Say if line(s) in record is/are rates.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isRate <- function(x) {
  return(grepl(pattern="^rate\\s*\\(.*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Say if line(s) in record is/are initial conditions.
#' 
#' @param x character vector
#' @return logical vector
#' @export
isInitialCondition <- function(x) {
  return(grepl(pattern="^[a-z_][a-z0-9_]+\\s*\\(\\s*0\\s*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Get initial condition compartment.
#' Assumes x is an initial condition (isInitialCondition already called).
#' 
#' @param x character vector
#' @return logical vector
#' @export
getInitialConditionCmt <- function(x) {
  return(gsub(pattern="([a-z_][a-z0-9_]+)(\\s*\\(\\s*0\\s*\\)\\s*=.*)", replacement="\\1", x=trim(x), ignore.case=TRUE))
}

#' Extract text between brackets.
#' 
#' @param x character value
#' @return text between brackets (trimmed)
#' @importFrom assertthat assert_that
#' @export
extractTextBetweenBrackets <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a character value to avoid ambiguities")
  retValue <- gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
  if (length(retValue) == 0) {
    stop(paste0("No parentheses found in ", x))
  }
  return(retValue[1] %>% trim())
}

#' Extract right-hand-side expression.
#' 
#' @param x character value
#' @return right-hand side expressionn
#' @importFrom assertthat assert_that
#' @export
extractRhs <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a character value to avoid ambiguities")
  tmp <- strsplit(x=x, split="=")[[1]]
  # Remove lhs and collapse (in case of several =)
  rhs <- paste0(tmp[-1], collapse="=")
  return(rhs)
}

#' Extract left-hand-side expression.
#' 
#' @param x character value
#' @return left-hand-side expression, not trimmed
#' @importFrom assertthat assert_that
#' @export
extractLhs <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a character value to avoid ambiguities")
  tmp <- strsplit(x=x, split="=")[[1]]
  lhs <- tmp[1]
  return(lhs)
}

#' Trim character vector. Remove all leading and trailing spaces.
#' 
#' @param x character vector
#' @return character vector without leading and trailing spaces
#' @importFrom assertthat assert_that
#' @export
trim <- function(x) {
  assertthat::assert_that(is.character(x), msg="x must be a character vector")
  return(gsub("^\\s+|\\s+$", "", x))
}