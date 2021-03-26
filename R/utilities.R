
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
  x <- trim(x)
  odePattern <- "^d/dt\\(.*\\)"
  return(grepl(pattern=odePattern, x=x, ignore.case=TRUE))
}

#' Get ODE name.
#' 
#' @param x character value
#' @return the ODE name, character value
#' @export
getODEName <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a character value to avoid ambiguities")
  x <- gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
  if (length(x) > 0) {
    return(trim(x[[1]]))
  } else {
    stop("x was not an ODE")
  }
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