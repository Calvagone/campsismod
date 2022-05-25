
#_______________________________________________________________________________
#----                             ode class                                 ----
#_______________________________________________________________________________

validateOde <- function(object) {
  if (startsWith(object@lhs, "A_")) {
    return(TRUE)
  } else {
    return("Derivative name must start with 'A_'")
  }
}

#'
#' ODE class. Any statement in the form d/dt(A_CMT) = B.
#'
#' @export
setClass(
  "ode",
  representation(
  ),
  contains = "equation",
  validity = validateOde
)

#'
#' Create a new ordinary differential equation (ODE).
#'
#' @param lhs left-hand side variable corresponding to derivative name, must start with 'A_'
#' @param rhs right-hand side expression corresponding to derivative value
#' @param comment comment if any, single character string
#' @return an ODE
#' @export
Ode <- function(lhs, rhs="", comment=as.character(NA)) {
  return(new("ode", lhs=lhs, rhs=rhs, comment=comment))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("ode"), definition = function(x) {
  return(paste0("ODE (", x@lhs, ")"))
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' @rdname toString
setMethod("toString", signature=c("ode"), definition=function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="campsis")
  model <- processExtraArg(args=list(...), name="model", default=CampsisModel())

  if (dest=="campsis" || dest=="rxode2") {
    retValue <- paste0("d/dt(", object@lhs, ")", "=", object@rhs)
  } else if (dest=="mrgsolve") {
    retValue <- paste0("dxdt_", object@lhs, "=", object@rhs, ";")
  } else if (dest=="NONMEM") {
    retValue <- paste0("DADT(", model %>% getCompartmentIndex(gsub("A_", "", object@lhs)), ")", "=", object@rhs)
  } else {
    UnsupportedDestException()
  }
  return(retValue %>% appendComment(object, dest))
})
