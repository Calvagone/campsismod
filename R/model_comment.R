
#_______________________________________________________________________________
#----                           comment class                               ----
#_______________________________________________________________________________

validateComment <- function(object) {
  return(TRUE)
}

#' 
#' Equation class. Any statement in the form A = B.
#' 
#' @slot comment a comment associated to this model statement
#' @export
setClass(
  "comment",
  representation(
  ),
  contains = "model_statement",
  validity = validateComment
)