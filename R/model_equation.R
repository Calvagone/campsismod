
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

#' 
#' Create a new equation.
#' 
#' @param lhs left-hand side variable corresponding to the assigned variable name
#' @param rhs right-hand side expression corresponding to a formula
#' @param comment comment if any, single character string
#' @return an equation
#' @export
Equation <- function(lhs, rhs, comment=as.character(NA)) {
  return(new("equation", lhs=lhs, rhs=rhs, comment=comment))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("equation"), definition = function(x) {
  return(paste0("EQUATION (", x@lhs, ")"))
})
