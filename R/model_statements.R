
#_______________________________________________________________________________
#----                       model_statements class                          ----
#_______________________________________________________________________________

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
  validity = function(object) {
    return(TRUE)
  }
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
