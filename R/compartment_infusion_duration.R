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
#' @param compartment compartment
#' @param rhs right-hand side part of the equation
#' @param rate logical value, TRUE if distribution is a rate, FALSE if it is an infusion
#' @return details about infusion duration/rate
#' @export
CompartmentInfusionDuration <- function(compartment, rhs, rate=FALSE) {
  return(new("compartment_infusion_duration", compartment=compartment, rhs=rhs, rate=rate))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("compartment_infusion_duration"), definition = function(x) {
  return(paste0("INFUSION_DURATION [", "CMT=", x@compartment@index, "]"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_bioavailability"), definition = function(object) {
  if (object@rate) {
    return("rate")
  } else {
    return("dur")
  }
})
