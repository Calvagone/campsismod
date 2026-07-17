#' Get the compartment index for the specified compartment name.
#' 
#' `getCompartmentIndex()` is deprecated in favor of `get_compartment_index()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_compartment_index
#' @return the corresponding compartment index
#' @export
#' @rdname getCompartmentIndex
getCompartmentIndex <- function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getCompartmentIndex()", "get_compartment_index()")
  get_compartment_index(object = object, name = name)
}

setGeneric("getCompartmentIndex", function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getCompartmentIndex()", "get_compartment_index()")
  get_compartment_index(object = object, name = name)
})

#' Get uncertainty on the parameters.
#' 
#' `getUncertainty()` is deprecated in favor of `get_uncertainty()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_uncertainty
#' @return data frame with standard error (se) and relative standard error (rse%) columns 
#' @export
#' @rdname getUncertainty
getUncertainty <- function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "getUncertainty()", "get_uncertainty()")
  get_uncertainty(object = object, ...)
}

setGeneric("getUncertainty", function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getUncertainty()", "get_uncertainty()")
  get_uncertainty(object = object, ...)
})
