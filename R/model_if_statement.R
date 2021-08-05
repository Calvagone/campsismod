
#_______________________________________________________________________________
#----                        if_statement class                             ----
#_______________________________________________________________________________

validateIfStatement <- function(object) {
  return(TRUE)
}

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
  validity = validateIfStatement
)