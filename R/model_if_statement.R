
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
    return(expectOne(object, "condition"))
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
IfStatement <- function(condition, equation, comment=as.character(NA)) {
  return(new("if_statement", condition=condition, equation=equation, comment=comment))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("if_statement"), definition = function(x) {
  return(paste0("IF (", x@condition, ") ", x@equation %>% getName()))
})

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

#' @rdname replaceAll
setMethod("replaceAll", signature=c("if_statement", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@condition <- object@condition %>% replaceAll(pattern=pattern, replacement=replacement, ...)
  object@equation <- object@equation %>% replaceAll(pattern=pattern, replacement=replacement, ...)
  return(object)
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("if_statement"), definition=function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="campsis")
  if (dest=="campsis" || isRxODE(dest) || dest=="mrgsolve") {
    retValue <- paste0("if (", object@condition, ") ", object@equation %>% toString(dest=dest, init=FALSE))
  } else if (dest=="NONMEM") {
    retValue <- paste0("IF (", object@condition, ") ", object@equation %>% toString(dest=dest, init=FALSE))
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})
