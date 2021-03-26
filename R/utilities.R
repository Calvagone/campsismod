
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