#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

validateInfusionRate <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "compartment_infusion_rate",
  representation(
  ),
  contains = "compartment_property",
  validity=validateInfusionRate
)

#'
#' Create an infusion rate.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return details about infusion rate
#' @export
InfusionRate <- function(compartment, rhs) {
  return(new("compartment_infusion_rate", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("compartment_infusion_rate"), definition = function(x) {
  return(paste0("INFUSION_RATE (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_infusion_rate"), definition = function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="RxODE")
  if (dest=="mrgsolve") {
    return("R")
  } else {
    return("rate")
  }
})
