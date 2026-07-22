
#_______________________________________________________________________________
#----                     compartment_lag_time class                        ----
#_______________________________________________________________________________

#' 
#' Compartment lag time class.
#' 
#' @export
setClass(
  "compartment_lag_time",
  representation(
  ),
  contains = "compartment_property",
  validity=function(object) {
    return(TRUE)
  }
)

#'
#' Create a lag time for the specified compartment.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return a lag time property
#' @export
LagTime <- function(compartment, rhs="") {
  return(new("compartment_lag_time", compartment=as.integer(compartment), rhs=rhs))
}

#_______________________________________________________________________________
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("compartment_lag_time"), definition = function(x) {
  return(paste0("LAG_TIME (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             get_prefix                                ----
#_______________________________________________________________________________

#' @rdname get_prefix
setMethod("get_prefix", signature = c("compartment_lag_time"), definition = function(object, ...) {
  dest <- process_extra_arg(args=list(...), name="dest", default="rxode2")
  if (dest=="mrgsolve") {
    return("ALAG")
  } else {
    return("lag")
  }
})

#_______________________________________________________________________________
#----                           get_record_name                               ----
#_______________________________________________________________________________

#' @rdname get_record_name
setMethod("get_record_name", signature = c("compartment_lag_time"), definition = function(object) {
  return("LAG")
})
