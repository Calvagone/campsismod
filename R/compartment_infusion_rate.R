#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

validateInfusionRate <- function(object) {
  return(TRUE)
}

#'
#' Compartment infusion rate class.
#'
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
#' @return an infusion rate property
#' @export
InfusionRate <- function(compartment, rhs="") {
  return(new("compartment_infusion_rate", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

#' @rdname getName
setMethod("getName", signature = c("compartment_infusion_rate"), definition = function(x) {
  return(paste0("RATE (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

#' @rdname getPrefix
setMethod("getPrefix", signature = c("compartment_infusion_rate"), definition = function(object, ...) {
  dest <- processExtraArg(args=list(...), name="dest", default="rxode2")
  if (dest=="mrgsolve") {
    return("R")
  } else {
    return("rate")
  }
})

#_______________________________________________________________________________
#----                           getRecordName                               ----
#_______________________________________________________________________________

#' @rdname getRecordName
setMethod("getRecordName", signature = c("compartment_infusion_rate"), definition = function(object) {
  return("RATE")
})
