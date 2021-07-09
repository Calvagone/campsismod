#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

validateInfusionDuration <- function(object) {
  return(TRUE)
}

#' 
#' Compartment infusion duration class.
#' 
#' @export
setClass(
  "compartment_infusion_duration",
  representation(
  ),
  contains = "compartment_property",
  validity=validateInfusionDuration
)

#'
#' Create an infusion duration.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return details about infusion duration
#' @export
InfusionDuration <- function(compartment, rhs) {
  return(new("compartment_infusion_duration", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("compartment_infusion_duration"), definition = function(x) {
  return(paste0("DURATION (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_infusion_duration"), definition = function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="RxODE")
  if (dest=="mrgsolve") {
    return("D")
  } else {
    return("dur")
  }
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

setMethod("getRecordName", signature = c("compartment_infusion_duration"), definition = function(object) {
  return("DURATION")
})
