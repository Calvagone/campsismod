#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

validateInfusionDuration <- function(object) {
  return(expectOneForAll(object, c("rate")))
}

#' @export
setClass(
  "compartment_infusion_duration",
  representation(
    rate = "logical"
  ),
  contains = "compartment_characteristic",
  prototype=prototype(rate=FALSE),
  validity=validateInfusionDuration
)

#'
#' Create an infusion duration/rate.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @param rate logical value, TRUE if distribution is a rate, FALSE if it is an infusion
#' @return details about infusion duration/rate
#' @export
CompartmentInfusionDuration <- function(compartment, rhs, rate=FALSE) {
  return(new("compartment_infusion_duration", compartment=as.integer(compartment), rhs=rhs, rate=rate))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("compartment_infusion_duration"), definition = function(x) {
  return(paste0("INFUSION_DURATION (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_infusion_duration"), definition = function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="RxODE")
  if (dest=="mrgsolve") {
    if (object@rate) {
      return("R")
    } else {
      return("D")
    }
  } else {
    if (object@rate) {
      return("rate")
    } else {
      return("dur")
    }
  }
})
