
#_______________________________________________________________________________
#----                        if_statement class                             ----
#_______________________________________________________________________________

validateIfStatement <- function(object) {
  return(expectOne(object, "condition"))
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

#' 
#' Create a new IF-statement.
#' 
#' @param condition condition, single character string
#' @param equation equation if condition is met
#' @param comment comment if any, single character string
#' @return an IF-statement
#' @export
IfStatement <- function(condition, equation, comment=as.character(NA)) {
  return(new("if_statement", condition=condition, equation=equation, comment=comment))
}

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
    retValue <- paste0("if (", object@condition, ") ", object@equation %>% toString(dest=dest, init=FALSE))
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})
