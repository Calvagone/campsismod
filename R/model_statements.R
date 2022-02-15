
#_______________________________________________________________________________
#----                       model_statements class                          ----
#_______________________________________________________________________________

validateModelStatements <- function(object) {
  return(TRUE)
}

#' 
#' Model statements class. A list of statements.
#' 
#' @export
setClass(
  "model_statements",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="model_statement"),
  validity = validateModelStatements
)

#' 
#' Create an empty list of model statements.
#' 
#' @return a model statements object
#' @export
ModelStatements <- function() {
  return(new("model_statements"))
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________


setMethod("show", signature=c("model_statements"), definition=function(object) {
  for (statement in object@list) {
    show(statement)
    cat("\n")
  }
})
