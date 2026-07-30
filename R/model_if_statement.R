#_______________________________________________________________________________
#----                        if_statement class                             ----
#_______________________________________________________________________________

#'
#' If-statement class. Any statement in the form if (condition) A = B.
#'
#' @slot condition IF statement condition
#' @slot equation any equation or ODE
#' @export
setClass(
  "if_statement",
  representation(
    condition = "character",
    equation = "equation"
  ),
  contains = "model_statement",
  validity = function(object) {
    return(expect_one(object, "condition"))
  }
)

#'
#' Create a new IF-statement.
#'
#' @param condition condition, single character string
#' @param equation equation if condition is met
#' @param comment comment if any, single character string
#' @return an IF-statement
#' @export
IfStatement <- function(condition, equation, comment = as.character(NA)) {
  return(new("if_statement", condition = condition, equation = equation, comment = comment))
}

#_______________________________________________________________________________
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("if_statement"), definition = function(x) {
  return(paste0("IF (", x@condition, ") ", x@equation %>% get_name()))
})

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' @rdname replace_all
setMethod(
  "replace_all",
  signature = c("if_statement", "pattern", "character"),
  definition = function(object, pattern, replacement, ...) {
    object@condition <- object@condition %>% replace_all(pattern = pattern, replacement = replacement, ...)
    object@equation <- object@equation %>% replace_all(pattern = pattern, replacement = replacement, ...)
    return(object)
  }
)

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature = c("if_statement"), definition = function(object, ...) {
  dest <- process_extra_arg(args = list(...), name = "dest", default = "campsis")
  if (dest == "campsis" || is_rxode(dest) || dest == "mrgsolve") {
    retValue <- paste0("if (", object@condition, ") ", object@equation %>% to_string(dest = dest, init = FALSE))
  } else if (dest == "NONMEM") {
    retValue <- paste0("IF (", object@condition, ") ", object@equation %>% to_string(dest = dest, init = FALSE))
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% append_comment(object, dest))
})
