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

#' Fill-in S4 object from the JSON content.
#' 
#' `loadFromJSON()` is deprecated in favor of `load_from_json()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams load_from_json
#' @return the loaded S4 object
#' @export
#' @rdname loadFromJSON
loadFromJSON <- function(object, json) {
  lifecycle::deprecate_warn("1.4.0", "loadFromJSON()", "load_from_json()")
  load_from_json(object = object, json = json)
}

setGeneric("loadFromJSON", function(object, json) {
  lifecycle::deprecate_warn("1.4.0", "loadFromJSON()", "load_from_json()")
  load_from_json(object = object, json = json)
})

#' Replace all occurrences in object.
#' 
#' `replaceAll()` is deprecated in favor of `replace_all()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams replace_all
#' @return the loaded S4 object
#' @export
#' @rdname replaceAll
replaceAll <- function(object, pattern, replacement, ...) {
  lifecycle::deprecate_warn("1.4.0", "replaceAll()", "replace_all()")
  replace_all(object = object, pattern = pattern, replacement = replacement, ...)
}

setGeneric("replaceAll", function(object, pattern, replacement, ...) {
  lifecycle::deprecate_warn("1.4.0", "replaceAll()", "replace_all()")
  replace_all(object = object, pattern = pattern, replacement = replacement, ...)
})

#' to_string generic method.
#' 
#' `toString()` is deprecated in favor of `to_string()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams to_string
#' @return character value/vector
#' @export
#' @rdname toString
toString <- function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "toString()", "to_string()")
  to_string(object = object, ...)
}

setGeneric("toString", function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "toString()", "to_string()")
  to_string(object = object, ...)
})
