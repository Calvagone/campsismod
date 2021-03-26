
#_______________________________________________________________________________
#----                     compartment_lag_time class                        ----
#_______________________________________________________________________________

validateLagTime <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "compartment_lag_time",
  representation(
  ),
  contains = "compartment_characteristic",
  validity=validateLagTime
)

#'
#' Create a lag time for the specified compartment.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return lag time
#' @export
CompartmentLagTime <- function(compartment, rhs) {
  return(new("compartment_lag_time", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________


setMethod("getName", signature = c("compartment_lag_time"), definition = function(x) {
  return(paste0("LAG_TIME (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             getPrefix                                ----
#_______________________________________________________________________________

setMethod("getPrefix", signature = c("compartment_lag_time"), definition = function(object) {
  return("lag")
})
