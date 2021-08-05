
#_______________________________________________________________________________
#----                          equation class                               ----
#_______________________________________________________________________________

validateEquation <- function(object) {
  return(expectOne(object, c("lhs", "rhs")))
}

#' 
#' Equation class. Any statement in the form A = B.
#' 
#' @slot lhs left-hand side expression
#' @slot rhs right-hand side expression
#' @export
setClass(
  "equation",
  representation(
    lhs = "character",
    rhs = "character"
  ),
  contains = "model_statement",
  validity = validateEquation
)