
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

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("equation"), definition=function(object, ...) {
  args <- list(...)
  dest <- processExtraArg(args=args, name="dest", default="campsis")
  init <- processExtraArg(args=args, name="init", default=TRUE)
  capture <- processExtraArg(args=args, name="init", default=FALSE)
  
  if (dest=="campsis" || dest=="RxODE") {
    retValue <- paste0(object@lhs, "=", object@rhs)
  } else if (dest=="mrgsolve") {
    retValue <- paste0(object@lhs, "=", object@rhs, ";")
    if (init) {
      retValue <- paste0("double ", retValue)
    } else if (capture) {
      retValue <- paste0("capture ", retValue)
    }
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})
