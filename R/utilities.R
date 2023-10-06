
#' Assert the given character vector is a single character string.
#' 
#' @param x single character string
#' @return no return value
#' @importFrom assertthat assert_that
#' @export
assertSingleCharacterString <- function(x) {
  assertthat::assert_that(is.character(x) && length(x)==1, msg="x must be a single character string")
}

#' Process extra arguments.
#' 
#' @param args arguments list
#' @param name argument name to retrieve
#' @param default default value if argument is not present
#' @param mandatory mandatory argument, logical value
#' @return requested argument value
#' @importFrom utils hasName
#' @export
processExtraArg <- function(args, name, default=NULL, mandatory=FALSE) {
  if (utils::hasName(args, name)) {
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
  return(grepl(pattern="^d/dt\\s*\\(.*\\)\\s*=", x=trim(x), ignore.case=TRUE))
}

#' Return the variable pattern (string form).
#' 
#' @return pattern (regular expression)
#' @keywords internal
#' 
variablePatternStr <- function() {
  return("[a-zA-Z_][a-zA-Z0-9_]*")
}

#' Return the variable pattern (string form), without the first character.
#' 
#' @return pattern (regular expression)
#' @keywords internal
#' 
variablePatternNoStartStr <- function() {
  return("[a-zA-Z0-9_]*")
}

#' Say if line in record is an equation not.
#' 
#' @param x character value
#' @return logical value
#' @export
isEquation <- function(x) {
  assertSingleCharacterString(x)
  parts <- strsplit(x, split="=")[[1]]
  if (length(parts) == 1) {
    return(FALSE)
  }
  variable <- parts[1] %>% trim()
  return(grepl(pattern=paste0("^", variablePatternStr(), "$"), x=variable))
}

#' Return the IF-statement pattern (string form).
#' 
#' @return pattern (regular expression)
#' @keywords internal
ifStatementPatternStr <- function() {
  return(paste0("if\\s*\\(.*\\)\\s*", variablePatternStr(), "\\s*="))
}

#' Say if line in record is an IF-statement.
#' 
#' @param x character value
#' @return logical value
#' @export
isIfStatement <- function(x) {
  return(grepl(pattern=paste0("^", ifStatementPatternStr()), x=trim(x), ignore.case=TRUE))
}

#' Extract text between brackets.
#' 
#' @param x character value
#' @return text between brackets (trimmed)
#' @export
extractTextBetweenBrackets <- function(x) {
  assertSingleCharacterString(x)
  retValue <- gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
  if (length(retValue) == 0) {
    stop(paste0("No parentheses found in ", x))
  }
  return(retValue[1] %>% trim())
}

#' Extract right-hand-side expression.
#' 
#' @param x character value
#' @param split character where to split
#' @return right-hand side expression
#' @export
extractRhs <- function(x, split="=") {
  assertSingleCharacterString(x)
  tmp <- strsplit(x=x, split=split)[[1]]
  # Remove lhs and collapse (in case of several =)
  rhs <- paste0(tmp[-1], collapse="=")
  return(rhs)
}

#' Extract left-hand-side expression.
#' 
#' @param x character value
#' @param split character where to split
#' @return left-hand-side expression, not trimmed
#' @export
extractLhs <- function(x, split="=") {
  assertSingleCharacterString(x)
  tmp <- strsplit(x=x, split=split)[[1]]
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

#' Check if string contains CAMPSIS-style comments.
#' 
#' @param x character vector
#' @return logical value
#' @export
hasComment <- function(x) {
  return(grepl("#", x=x, fixed=TRUE))
}

#' Check if string is a CAMPSIS comment (i.e. not an equation).
#' 
#' @param x character vector
#' @return logical value
#' @export
isComment <- function(x) {
  return(grepl("^\\s*#", x=x))
}

#' Check if string is an empty line.
#' 
#' @param x character vector
#' @return logical value
#' @export
isEmptyLine <- function(x) {
  return(grepl("^\\s*$", x=x))
}

#' Is strict record delimiter. A strict record delimiter is any line starting 
#' with [...] and followed by nothing but spaces or a possible comment.
#' 
#' @param line any line, single character value
#' @return a logical value
#' @export
isStrictRecordDelimiter <- function(line) {
  return(grepl("^\\s*\\[.*\\]((\\s*)|(\\s*#.*))$", line))
}

#' Is record delimiter. A record delimiter is any line starting with [...].
#' 
#' @param line any line, single character value
#' @return a logical value
#' @export
isRecordDelimiter <- function(line) {
  return(grepl("^\\s*\\[.*\\].*$", line))
}

#' Get record delimiter.
#' 
#' @param line any line, single character value
#' @return the record delimiter between brackets
#' @export
getRecordDelimiter <- function(line) {
  return(gsub("\\[(.*)\\](.*)","\\1", line) %>% trim())
}

#' Get record equation names
#' 
#' @param record any code record
#' @return a character vector with the equation names
#' @export
#' @keywords internal
getRecordEquationNames <- function(record) {
  retValue <- NULL
  for (statement in record@statements@list) {
    if (is(statement, "equation") && !(is(statement, "ode"))) {
      retValue <- c(retValue, statement@lhs)
    }
  }
  return(retValue)
}

#' Check is vector has NA's only.
#' 
#' @param x any vector
#' @return TRUE if all values are NA, FALSE otherwise
#' @export
#' @keywords internal
allNa <- function(x) {
  return(all(is.na(x)))
}

#' Remove given column(s) if it has only NA's.
#' 
#' @param x any data frame
#' @param column column name(s)
#' @return updated data frame
#' @importFrom dplyr any_of where
#' @export
#' @keywords internal
removeNaColumn <- function(x, column) {
  return(x %>% dplyr::select(!(dplyr::any_of(column) & dplyr::where(allNa))))
}
