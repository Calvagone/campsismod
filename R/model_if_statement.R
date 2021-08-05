
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

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("if_statement"), definition = function(x) {
  return(as.character(NA)) # IF statement non-identifiable 
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("if_statement"), definition=function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="campsis")
  if (dest=="campsis" || dest=="RxODE" || dest=="mrgsolve") {
    retValue <- paste0("if (", object@condition, ")", object@equation %>% toString(dest=dest, init=FALSE))
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})