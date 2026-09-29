#_______________________________________________________________________________
#----                   compartment_infusion_duration class                 ----
#_______________________________________________________________________________

#'
#' Compartment infusion rate class.
#'
#' @export
setClass(
  "compartment_infusion_rate",
  representation(),
  contains = "compartment_property",
  validity = function(object) {
    return(TRUE)
  }
)

#'
#' Create an infusion rate.
#'
#' @param compartment compartment index
#' @param rhs right-hand side part of the equation
#' @return an infusion rate property
#' @export
InfusionRate <- function(compartment, rhs = "") {
  return(new("compartment_infusion_rate", compartment = as.integer(compartment), rhs = rhs))
}

#_______________________________________________________________________________
#----                            get_name                                    ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature = c("compartment_infusion_rate"), definition = function(x) {
  return(paste0("RATE (", "CMT=", x@compartment, ")"))
})

#_______________________________________________________________________________
#----                             get_prefix                                ----
#_______________________________________________________________________________

#' @rdname get_prefix
setMethod("get_prefix", signature = c("compartment_infusion_rate"), definition = function(object, ...) {
  dest <- process_extra_arg(args = list(...), name = "dest", default = "rxode2")
  if (dest == "mrgsolve") {
    return("R")
  } else {
    return("rate")
  }
})

#_______________________________________________________________________________
#----                           get_record_name                               ----
#_______________________________________________________________________________

#' @rdname get_record_name
setMethod("get_record_name", signature = c("compartment_infusion_rate"), definition = function(object) {
  return("RATE")
})
