#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

#'
#' Compartment infusion duration class.
#'
#' @export
setClass(
  "compartment_infusion_duration",
  representation(),
  contains = "compartment_property",
  validity = function(object) {
    return(TRUE)
  }
)

#'
#' Create an infusion duration.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return an infusion duration property
#' @export
InfusionDuration <- function(compartment, rhs = "") {
  return(new("compartment_infusion_duration", compartment = as.integer(compartment), rhs = rhs))
}

#_______________________________________________________________________________
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("compartment_infusion_duration"), definition = function(x) {
  return(paste0("DURATION (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             get_prefix                                ----
#_______________________________________________________________________________

#' @rdname get_prefix
setMethod("get_prefix", signature = c("compartment_infusion_duration"), definition = function(object, ...) {
  dest <- process_extra_arg(args = list(...), name = "dest", default = "rxode2")
  if (dest == "mrgsolve") {
    return("D")
  } else {
    return("dur")
  }
})

#_______________________________________________________________________________
#----                           get_record_name                               ----
#_______________________________________________________________________________

#' @rdname get_record_name
setMethod("get_record_name", signature = c("compartment_infusion_duration"), definition = function(object) {
  return("DURATION")
})
